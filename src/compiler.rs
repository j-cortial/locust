use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

use crate::{
    chunk::{
        Chunk, OP_ADD, OP_CONSTANT, OP_DIVIDE, OP_EQUAL, OP_FALSE, OP_GREATER, OP_LESS,
        OP_MULTIPLY, OP_NEGATE, OP_NIL, OP_NOT, OP_RETURN, OP_SUBTRACT, OP_TRUE,
    },
    debug::disassemble,
    scanner::{Scanner, Token, TokenType},
    value::Value,
};

use std::{ops::Add, str::from_utf8};

struct Parser<'s, 'a: 's> {
    scanner: &'a mut Scanner<'s>,
    current: Option<Token<'a>>,
    previous: Option<Token<'a>>,
    had_error: bool,
    panic_mode: bool,
}

impl<'s, 'a: 's> Parser<'s, 'a> {
    fn new(scanner: &'a mut Scanner<'s>) -> Self {
        Self {
            scanner,
            current: None,
            previous: None,
            had_error: false,
            panic_mode: false,
        }
    }

    fn advance(&mut self) {
        self.previous = self.current;
        loop {
            let token = self.scanner.scan_token();
            self.current = Some(token);
            if self.current.unwrap().kind != TokenType::Error {
                break;
            }
            let message = from_utf8(self.current.unwrap().span).unwrap();
            self.error_at_current(message);
        }
    }

    fn error_at_current(&mut self, message: &str) {
        self.error_at(&self.current.unwrap(), message);
    }

    fn error(&mut self, message: &str) {
        self.error_at(&self.previous.unwrap(), message);
    }

    fn error_at(&mut self, token: &Token, message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprint!("[line {}] Error", token.line);
        match token.kind {
            TokenType::Eof => {
                eprint!(" at end");
            }
            TokenType::Error => {}
            _ => {
                eprint!(" at '{}'", from_utf8(token.span).unwrap());
            }
        }
        eprintln!(": {}", message);
        self.had_error = true;
    }

    fn consume(&mut self, kind: TokenType, message: &str) {
        if self.current.unwrap().kind == kind {
            self.advance();
            return;
        }
        self.error_at_current(message);
    }

    fn emit_byte(&mut self, current_chunk: &mut Chunk, byte: u8) {
        current_chunk.write(byte, self.previous.unwrap().line);
    }

    fn emit_bytes(&mut self, current_chunk: &mut Chunk, byte1: u8, byte2: u8) {
        self.emit_byte(current_chunk, byte1);
        self.emit_byte(current_chunk, byte2);
    }

    fn emit_return(&mut self, current_chunk: &mut Chunk) {
        self.emit_byte(current_chunk, OP_RETURN);
    }

    fn make_constant(&mut self, current_chunk: &mut Chunk, value: Value) -> u8 {
        let constant = current_chunk.add_constant(value);
        if constant > u8::MAX as usize {
            self.error("Too many constants in one chunk");
            return 0;
        }
        constant as u8
    }

    fn emit_constant(&mut self, current_chunk: &mut Chunk, value: Value) {
        let constant_location = self.make_constant(current_chunk, value);
        self.emit_bytes(current_chunk, OP_CONSTANT, constant_location);
    }

    fn end_compiler(&mut self, current_chunk: &mut Chunk) {
        self.emit_return(current_chunk);
        #[cfg(feature = "debug_print_code")]
        {
            if !self.had_error {
                disassemble(current_chunk, "code");
            }
        }
    }

    fn binary(&mut self, current_chunk: &mut Chunk) {
        let operator_type = self.previous.unwrap().kind;
        let rule = get_rule(operator_type);
        self.parse_precedence(current_chunk, rule.precedence + 1);
        match operator_type {
            TokenType::BangEqual => self.emit_bytes(current_chunk, OP_EQUAL, OP_NOT),
            TokenType::EqualEqual => self.emit_byte(current_chunk, OP_EQUAL),
            TokenType::Greater => self.emit_byte(current_chunk, OP_GREATER),
            TokenType::GreaterEqual => self.emit_bytes(current_chunk, OP_LESS, OP_NOT),
            TokenType::Less => self.emit_byte(current_chunk, OP_LESS),
            TokenType::LessEqual => self.emit_bytes(current_chunk, OP_GREATER, OP_NOT),
            TokenType::Plus => self.emit_byte(current_chunk  , OP_ADD),
            TokenType::Minus => self.emit_byte(current_chunk, OP_SUBTRACT),
            TokenType::Star => self.emit_byte(current_chunk, OP_MULTIPLY),
            TokenType::Slash => self.emit_byte(current_chunk, OP_DIVIDE),
            _ => {
                unreachable!()
            }
        };
    }

    fn literal(&mut self, current_chunk: &mut Chunk) {
        match self.previous.unwrap().kind {
            TokenType::False => self.emit_byte(current_chunk, OP_FALSE),
            TokenType::Nil => self.emit_byte(current_chunk, OP_NIL),
            TokenType::True => self.emit_byte(current_chunk, OP_TRUE),
            _ => unreachable!(),
        }
    }

    fn grouping(&mut self, current_chunk: &mut Chunk) {
        self.expression(current_chunk);
        self.consume(TokenType::RightParen, "Expect '(' after expression");
    }

    fn number(&mut self, current_chunk: &mut Chunk) {
        let value: f64 = from_utf8(self.previous.unwrap().span)
            .unwrap()
            .parse()
            .unwrap();
        self.emit_constant(current_chunk, Value::Number(value));
    }

    fn unary(&mut self, current_chunk: &mut Chunk) {
        let operator_type = self.previous.unwrap().kind;
        self.parse_precedence(current_chunk, Precedence::Unary);
        match operator_type {
            TokenType::Bang => {
                self.emit_byte(current_chunk, OP_NOT);
            }
            TokenType::Minus => {
                self.emit_byte(current_chunk, OP_NEGATE);
            }
            _ => {
                unreachable!()
            }
        }
    }

    fn parse_precedence(&mut self, current_chunk: &mut Chunk, precedence: Precedence) {
        self.advance();
        let prefix_rule = get_rule(self.previous.unwrap().kind).prefix;
        match prefix_rule {
            None => self.error("Expect expression"),
            Some(f) => {
                f(self, current_chunk);
                while precedence <= get_rule(self.current.unwrap().kind).precedence {
                    self.advance();
                    let infix_rule = get_rule(self.previous.unwrap().kind).infix;
                    infix_rule.unwrap()(self, current_chunk);
                }
            }
        }
    }

    fn expression(&mut self, current_chunk: &mut Chunk) {
        self.parse_precedence(current_chunk, Precedence::Assignment);
    }
}

fn get_rule<'a, 'b, 'c, 'd>(token_type: TokenType) -> ParseRule<'a, 'b, 'c, 'd> {
    let rules: [ParseRule; 40] = [
        parse_rule(Some(Parser::grouping), None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(Some(Parser::unary), Some(Parser::binary), Precedence::Term),
        parse_rule(None, Some(Parser::binary), Precedence::Term),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, Some(Parser::binary), Precedence::Factor),
        parse_rule(None, Some(Parser::binary), Precedence::Factor),
        parse_rule(Some(Parser::unary), None, Precedence::None),
        parse_rule(None, Some(Parser::binary), Precedence::Equality),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, Some(Parser::binary), Precedence::Equality),
        parse_rule(None, Some(Parser::binary), Precedence::Comparison),
        parse_rule(None, Some(Parser::binary), Precedence::Comparison),
        parse_rule(None, Some(Parser::binary), Precedence::Comparison),
        parse_rule(None, Some(Parser::binary), Precedence::Comparison),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(Some(Parser::number), None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(Some(Parser::literal), None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(Some(Parser::literal), None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(Some(Parser::literal), None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
    ];
    rules[token_type as usize]
}

pub fn compile(source: &str, chunk: &mut Chunk) -> bool {
    let mut scanner = Scanner::new(source.as_bytes());
    let mut parser = Parser::new(&mut scanner);
    let compiling_chunk = chunk;
    parser.advance();
    parser.expression(compiling_chunk);
    parser.consume(TokenType::Eof, "Expect end of expression");
    parser.end_compiler(compiling_chunk);
    parser.had_error
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, ToPrimitive, FromPrimitive)]
enum Precedence {
    None = 0,
    Assignment, // =
    Or,         // or
    And,        // And,
    Equality,   // ==
    Comparison, // < > <= <=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Add<i32> for Precedence {
    type Output = Self;

    fn add(self, rhs: i32) -> Self::Output {
        FromPrimitive::from_i32(self.to_i32().unwrap() + rhs).unwrap()
    }
}

#[derive(Clone, Copy)]
struct ParseRule<'a, 'b, 'c, 'd> {
    prefix: ParseFn<'a, 'b, 'c, 'd>,
    infix: ParseFn<'a, 'b, 'c, 'd>,
    precedence: Precedence,
}

const fn parse_rule<'a, 'b, 'c, 'd>(
    prefix: ParseFn<'a, 'b, 'c, 'd>,
    infix: ParseFn<'a, 'b, 'c, 'd>,
    precedence: Precedence,
) -> ParseRule<'a, 'b, 'c, 'd> {
    ParseRule {
        prefix,
        infix,
        precedence,
    }
}

type ParseFn<'a, 'b, 'c, 'd> = Option<fn(&'a mut Parser<'c, 'd>, &'b mut Chunk)>;

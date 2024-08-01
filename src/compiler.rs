use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

use crate::{
    chunk::{
        OP_ADD, OP_CONSTANT, OP_DEFINE_GLOBAL, OP_DIVIDE, OP_EQUAL, OP_FALSE, OP_GET_GLOBAL,
        OP_GET_LOCAL, OP_GREATER, OP_JUMP, OP_JUMP_IF_FALSE, OP_LESS, OP_LOOP, OP_MULTIPLY,
        OP_NEGATE, OP_NIL, OP_NOT, OP_POP, OP_PRINT, OP_RETURN, OP_SET_GLOBAL, OP_SET_LOCAL,
        OP_SUBTRACT, OP_TRUE,
    },
    debug::disassemble,
    object::{Intern, ObjFunction, ObjString},
    scanner::{Scanner, Token, TokenType},
    value::Value,
};

use std::{mem, ops::Add, rc::Rc, str::from_utf8};

struct Parser<'s, 'a: 's> {
    scanner: &'a mut Scanner<'s>,
    compiler: Box<Compiler<'s>>,
    intern: &'a mut dyn Intern,
    current: Option<Token<'a>>,
    previous: Option<Token<'a>>,
    had_error: bool,
    panic_mode: bool,
}

impl<'s, 'a: 's> Parser<'s, 'a> {
    fn new(
        scanner: &'a mut Scanner<'s>,
        compiler: Box<Compiler<'s>>,
        intern: &'a mut dyn Intern,
    ) -> Self {
        Self {
            scanner,
            compiler,
            intern,
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

    fn check(&self, kind: TokenType) -> bool {
        self.current.unwrap().kind == kind
    }

    fn match_token(&mut self, kind: TokenType) -> bool {
        if !self.check(kind) {
            return false;
        }
        self.advance();
        true
    }

    fn emit_byte(&mut self, byte: u8) {
        let current_chunk = &mut self.compiler.function.chunk;
        current_chunk.write(byte, self.previous.unwrap().line);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(OP_LOOP);

        let offset = {
            let current_chunk = &mut self.compiler.function.chunk;
            current_chunk.count() - loop_start + 2
        };
        if offset > u16::MAX as usize {
            self.error("Loop body too large");
        }

        self.emit_byte((offset >> 8 & 0xff) as u8);
        self.emit_byte((offset & 0xff) as u8);
    }

    fn emit_jump(&mut self, instruction: u8) -> usize {
        self.emit_byte(instruction);
        self.emit_byte(0xff);
        self.emit_byte(0xff);

        let current_chunk = &mut self.compiler.function.chunk;
        current_chunk.count() - 2
    }

    fn emit_return(&mut self) {
        self.emit_byte(OP_RETURN);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        let constant = {
            let current_chunk = &mut self.compiler.function.chunk;
            current_chunk.add_constant(value)
        };
        if constant > u8::MAX as usize {
            self.error("Too many constants in one chunk");
            return 0;
        }
        constant as u8
    }

    fn emit_constant(&mut self, value: Value) {
        let constant_location = self.make_constant(value);
        self.emit_bytes(OP_CONSTANT, constant_location);
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = {
            let current_chunk = &mut self.compiler.function.chunk;
            // -2 to adjust for the bytecode for the jump offset itself
            current_chunk.count() - offset - 2
        };
        if jump > u16::MAX as usize {
            self.error("Too much code to jump over");
        }
        let current_chunk = &mut self.compiler.function.chunk;
        current_chunk[offset] = ((jump >> 8) & 0xff) as u8;
        current_chunk[offset + 1] = (jump & 0xff) as u8;
    }

    fn end_compiler(&mut self) -> Box<ObjFunction> {
        self.emit_return();

        let mut function = Box::new(ObjFunction::new());
        mem::swap(&mut self.compiler.function, &mut function);

        #[cfg(feature = "debug_print_code")]
        {
            if !self.had_error {
                let current_chunk = &mut function.chunk;
                let function_name = match function.name {
                    None => "<script>",
                    Some(ref s) => &s.content,
                };
                disassemble(current_chunk, function_name);
            }
        }

        if let Some(enclosing_compiler) = self.compiler.enclosing.take() {
            self.compiler = enclosing_compiler;
        }

        function
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;
        while let Some(l) = self.compiler.locals.last() {
            if l.depth > self.compiler.scope_depth {
                self.emit_byte(OP_POP);
                self.compiler.locals.pop();
            } else {
                break;
            }
        }
    }

    fn binary(&mut self, _can_assign: bool) {
        let operator_type = self.previous.unwrap().kind;
        let rule = get_rule(operator_type);
        self.parse_precedence(rule.precedence + 1);
        match operator_type {
            TokenType::BangEqual => self.emit_bytes(OP_EQUAL, OP_NOT),
            TokenType::EqualEqual => self.emit_byte(OP_EQUAL),
            TokenType::Greater => self.emit_byte(OP_GREATER),
            TokenType::GreaterEqual => self.emit_bytes(OP_LESS, OP_NOT),
            TokenType::Less => self.emit_byte(OP_LESS),
            TokenType::LessEqual => self.emit_bytes(OP_GREATER, OP_NOT),
            TokenType::Plus => self.emit_byte(OP_ADD),
            TokenType::Minus => self.emit_byte(OP_SUBTRACT),
            TokenType::Star => self.emit_byte(OP_MULTIPLY),
            TokenType::Slash => self.emit_byte(OP_DIVIDE),
            _ => {
                unreachable!()
            }
        };
    }

    fn literal(&mut self, _can_assign: bool) {
        match self.previous.unwrap().kind {
            TokenType::False => self.emit_byte(OP_FALSE),
            TokenType::Nil => self.emit_byte(OP_NIL),
            TokenType::True => self.emit_byte(OP_TRUE),
            _ => unreachable!(),
        }
    }

    fn grouping(&mut self, _can_assign: bool) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect '(' after expression");
    }

    fn number(&mut self, _can_assign: bool) {
        let value: f64 = from_utf8(self.previous.unwrap().span)
            .unwrap()
            .parse()
            .unwrap();
        self.emit_constant(Value::Number(value));
    }

    fn or(&mut self, _can_assign: bool) {
        let else_jump = self.emit_jump(OP_JUMP_IF_FALSE);
        let end_jump = self.emit_jump(OP_JUMP);
        self.patch_jump(else_jump);
        self.emit_byte(OP_POP);
        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn string(&mut self, _can_assign: bool) {
        let token_span = self.previous.unwrap().span;
        let obj_string = ObjString::from_u8(self.intern, &token_span[1..token_span.len() - 1]);
        self.emit_constant(Value::from_obj(obj_string));
    }

    fn named_variable(&mut self, name: Token, can_assign: bool) {
        let (arg, ok) = self.compiler.resolve_local(&name);
        if !ok {
            self.error("Cannot read local variable in its own initializer");
        }
        let (get_op, set_op, arg) = if arg != -1 {
            (OP_GET_LOCAL, OP_SET_LOCAL, arg)
        } else {
            (
                OP_GET_GLOBAL,
                OP_SET_GLOBAL,
                self.identifier_constant(&name) as Depth,
            )
        };
        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            self.emit_bytes(set_op, arg as u8);
        } else {
            self.emit_bytes(get_op, arg as u8);
        }
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.previous.unwrap(), can_assign);
    }

    fn unary(&mut self, _can_assign: bool) {
        let operator_type = self.previous.unwrap().kind;
        self.parse_precedence(Precedence::Unary);
        match operator_type {
            TokenType::Bang => {
                self.emit_byte(OP_NOT);
            }
            TokenType::Minus => {
                self.emit_byte(OP_NEGATE);
            }
            _ => {
                unreachable!()
            }
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let prefix_rule = get_rule(self.previous.unwrap().kind).prefix;
        match prefix_rule {
            None => self.error("Expect expression"),
            Some(prefix_rule) => {
                let can_assign = precedence <= Precedence::Assignment;
                prefix_rule(self, can_assign);
                while precedence <= get_rule(self.current.unwrap().kind).precedence {
                    self.advance();
                    let infix_rule = get_rule(self.previous.unwrap().kind).infix.unwrap();
                    infix_rule(self, can_assign);
                }
                if can_assign && self.match_token(TokenType::Equal) {
                    self.error("Invalid assignment target")
                }
            }
        }
    }

    fn identifier_constant(&mut self, name: &Token) -> u8 {
        let token_span = name.span;
        let value = ObjString::from_u8(self.intern, token_span);
        self.make_constant(Value::from_obj(value))
    }

    fn add_local(&mut self, name: &Token<'s>) {
        if self.compiler.locals.len() == UINT8_COUNT {
            self.error("Too many local variables in function");
            return;
        }
        self.compiler.locals.push(Local::new(*name, -1));
    }

    fn declare_variable(&mut self) {
        let scope_depth = self.compiler.scope_depth;
        if scope_depth == 0 {
            return;
        }
        let name = &self.previous.unwrap();
        let local = self
            .compiler
            .locals
            .iter()
            .rev()
            .find(|&l| l.depth != -1 && l.depth < scope_depth);
        if let Some(local) = local {
            if name.identifier_equal(&local.name) {
                self.error("Already a variable with this name in this scope");
            }
        }
        self.add_local(name);
    }

    fn parse_variable(&mut self, error_message: &str) -> u8 {
        self.consume(TokenType::Identifier, error_message);
        self.declare_variable();
        if self.compiler.scope_depth > 0 {
            return 0;
        }
        self.identifier_constant(&self.previous.unwrap())
    }

    fn define_variable(&mut self, global: u8) {
        if self.compiler.scope_depth > 0 {
            self.compiler.mark_initialized();
            return;
        }
        self.emit_bytes(OP_DEFINE_GLOBAL, global);
    }

    fn and(&mut self, _can_assign: bool) {
        let end_jump = self.emit_jump(OP_JUMP_IF_FALSE);
        self.emit_byte(OP_POP);
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block");
    }

    fn function(&mut self, kind: FunctionType) {
        let mut compiler = Box::new(Compiler::new(kind, None));
        mem::swap(&mut self.compiler, &mut compiler);
        self.compiler.enclosing = Some(compiler);

        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after function name");
        self.consume(TokenType::RightParen, "Expect ')' after parameters");
        self.consume(TokenType::LeftBrace, "Expect '{' before function body");
        self.block();

        let function = self.end_compiler();
        let function = self.make_constant(Value::from_obj(Rc::<ObjFunction>::from(function)));
        self.emit_bytes(OP_CONSTANT, function);
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function name");
        self.compiler.mark_initialized();
        self.function(FunctionType::Function);
        self.define_variable(global);
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name");
        if self.match_token(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_byte(OP_NIL);
        }
        self.consume(
            TokenType::SemiColon,
            "Expect ';' after variable declaration",
        );
        self.define_variable(global);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::SemiColon, "Expect ';' after expression");
        self.emit_byte(OP_POP);
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'");
        if self.match_token(TokenType::SemiColon) {
            // No initializer
        } else if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = {
            let current_chunk = &mut self.compiler.function.chunk;
            current_chunk.count()
        };
        let exit_jump = if self.match_token(TokenType::SemiColon) {
            None
        } else {
            self.expression();
            self.consume(TokenType::SemiColon, "Expect ';' after loop condition");

            // Jump out of the loop if the condition is false
            let exit_jump = self.emit_jump(OP_JUMP_IF_FALSE);
            self.emit_byte(OP_POP);
            Some(exit_jump)
        };

        if !self.match_token(TokenType::RightParen) {
            let body_jump = self.emit_jump(OP_JUMP);
            let increment_start = {
                let current_chunk = &mut self.compiler.function.chunk;
                current_chunk.count()
            };
            self.expression();
            self.emit_byte(OP_POP);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit_byte(OP_POP); // Condition
        }

        self.end_scope();
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition");
        let then_jump = self.emit_jump(OP_JUMP_IF_FALSE);
        self.emit_byte(OP_POP);
        self.statement();
        let else_jump = self.emit_jump(OP_JUMP);
        self.patch_jump(then_jump);
        self.emit_byte(OP_POP);
        if self.match_token(TokenType::Else) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::SemiColon, "Expect ';' after value");
        self.emit_byte(OP_PRINT);
    }

    fn while_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'");
        self.expression();
        let loop_start = {
            let current_chunk = &mut self.compiler.function.chunk;
            current_chunk.count()
        };
        self.consume(TokenType::RightParen, "Expect ')' after condition");

        let exit_jump = self.emit_jump(OP_JUMP_IF_FALSE);
        self.emit_byte(OP_POP);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_byte(OP_POP);
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while self.current.unwrap().kind != TokenType::Eof {
            if self.previous.unwrap().kind == TokenType::SemiColon {
                return;
            }
            match self.current.unwrap().kind {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => {
                    return;
                }
                _ => {
                    // Do nothing
                }
            }
            self.advance();
        }
    }

    fn declaration(&mut self) {
        if self.match_token(TokenType::Fun) {
            self.fun_declaration();
        } else if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }
        if self.panic_mode {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        if self.match_token(TokenType::Print) {
            self.print_statement();
        } else if self.match_token(TokenType::For) {
            self.for_statement();
        } else if self.match_token(TokenType::If) {
            self.if_statement();
        } else if self.match_token(TokenType::While) {
            self.while_statement();
        } else if self.match_token(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }
}

fn get_rule<'a, 'c, 'd>(token_type: TokenType) -> ParseRule<'a, 'c, 'd> {
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
        parse_rule(Some(Parser::variable), None, Precedence::None),
        parse_rule(Some(Parser::string), None, Precedence::None),
        parse_rule(Some(Parser::number), None, Precedence::None),
        parse_rule(None, Some(Parser::and), Precedence::And),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(Some(Parser::literal), None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(None, None, Precedence::None),
        parse_rule(Some(Parser::literal), None, Precedence::None),
        parse_rule(None, Some(Parser::or), Precedence::Or),
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

pub fn compile(source: &str, intern: &mut dyn Intern) -> Option<Box<ObjFunction>> {
    let mut scanner = Scanner::new(source.as_bytes());
    let mut compiler = Box::new(Compiler::new(FunctionType::Script, None));
    let mut parser = Parser::new(&mut scanner, compiler, intern);
    parser.advance();
    while !parser.match_token(TokenType::Eof) {
        parser.declaration();
    }
    let res = parser.end_compiler();
    if parser.had_error {
        None
    } else {
        Some(res)
    }
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
struct ParseRule<'a, 'c, 'd> {
    prefix: ParseFn<'a, 'c, 'd>,
    infix: ParseFn<'a, 'c, 'd>,
    precedence: Precedence,
}

const fn parse_rule<'a, 'c, 'd>(
    prefix: ParseFn<'a, 'c, 'd>,
    infix: ParseFn<'a, 'c, 'd>,
    precedence: Precedence,
) -> ParseRule<'a, 'c, 'd> {
    ParseRule {
        prefix,
        infix,
        precedence,
    }
}

type ParseFn<'a, 'c, 'd> = Option<fn(&'a mut Parser<'c, 'd>, bool)>;

type Depth = i32;
#[derive(Debug)]
struct Local<'t> {
    name: Token<'t>,
    depth: Depth,
}

impl<'t> Local<'t> {
    fn new(name: Token<'t>, depth: Depth) -> Self {
        Self { name, depth }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionType {
    Function,
    Script,
}

const UINT8_COUNT: usize = 256;

#[derive(Debug)]
struct Compiler<'l> {
    enclosing: Option<Box<Compiler<'l>>>,
    function: Box<ObjFunction>,
    function_type: FunctionType,
    locals: Vec<Local<'l>>,
    scope_depth: Depth,
}

impl<'l> Compiler<'l> {
    fn new(function_type: FunctionType, enclosing: Option<Box<Compiler<'l>>>) -> Self {
        Self {
            enclosing,
            function: Box::new(ObjFunction::new()),
            function_type,
            locals: vec![Local::new(
                Token {
                    kind: TokenType::Eof,
                    span: b"",
                    line: 0,
                },
                0,
            )],
            scope_depth: Default::default(),
        }
    }

    fn resolve_local(&self, name: &Token<'l>) -> (Depth, bool) {
        self.locals
            .iter()
            .rev()
            .enumerate()
            .find(|&(_, l)| l.name.identifier_equal(name))
            .map_or((-1, true), |(i, l)| {
                ((self.locals.len() - i) as i32 - 1, l.depth != -1)
            })
    }

    fn mark_initialized(&mut self) {
        if self.scope_depth == 0 {
            return;
        }
        self.locals.last_mut().map(|l| l.depth = self.scope_depth);
    }
}

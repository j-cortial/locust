use crate::{
    chunk::Chunk,
    scanner::{Scanner, Token, TokenType},
};

use std::str::from_utf8;

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
    }

    fn consume(&mut self, kind: TokenType, message: &str) {
        if self.current.unwrap().kind == kind {
            self.advance();
            return;
        }
        self.error_at_current(message);
    }
}

pub fn compile(source: &str, chunk: &mut Chunk) -> bool {
    let mut scanner = Scanner::new(source.as_bytes());
    let mut parser = Parser::new(&mut scanner);
    parser.advance();
    todo!(); //parser.expression();
    parser.consume(TokenType::Eof, "Expect end of expression");
    parser.had_error
}

use crate::scanner::{Scanner, Token, TokenType};

use std::str::from_utf8;

pub fn compile(source: &str) {
    let mut scanner = Scanner::new(source.as_bytes());
    let mut line = -1;
    loop {
        let token = scanner.scan_token();
        if token.line != line {
            print!("{:4} ", token.line);
            line = token.line;
        } else {
            print!("   | ");
        }
        println!("{:2} '{}'", token.kind as u8, from_utf8(token.span).unwrap());
        if token.kind == TokenType::Eof {
            break;
        }
    }
}

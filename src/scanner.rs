#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a [u8],
    start: usize,
    current: usize,
    line: i32,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }
}

impl<'t, 'a: 't> Scanner<'a> {
    pub fn scan_token(&mut self) -> Token<'t> {
        self.skip_whitespace();
        self.start = self.current;
        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }
        let c = self.advance();
        if is_alpha(c) {
            return self.identifier();
        }
        if c.is_ascii_digit() {
            return self.number();
        }
        match c {
            b'(' => self.make_token(TokenType::LeftParen),
            b')' => self.make_token(TokenType::RightParen),
            b'{' => self.make_token(TokenType::LeftBrace),
            b'}' => self.make_token(TokenType::RightBrace),
            b';' => self.make_token(TokenType::SemiColon),
            b',' => self.make_token(TokenType::Comma),
            b'.' => self.make_token(TokenType::Dot),
            b'-' => self.make_token(TokenType::Minus),
            b'+' => self.make_token(TokenType::Plus),
            b'/' => self.make_token(TokenType::Slash),
            b'*' => self.make_token(TokenType::Star),
            b'!' => {
                let kind = if self.match_token(b'=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.make_token(kind)
            }
            b'=' => {
                let kind = if self.match_token(b'=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.make_token(kind)
            }
            b'<' => {
                let kind = if self.match_token(b'=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.make_token(kind)
            }
            b'>' => {
                let kind = if self.match_token(b'=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.make_token(kind)
            }
            b'"' => self.string(),
            _ => self.error_token(b"Unexpected character"),
        }
    }

    fn make_token(&self, kind: TokenType) -> Token<'t> {
        Token {
            kind,
            span: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    fn error_token(&self, message: &'static [u8]) -> Token<'t> {
        Token {
            kind: TokenType::Error,
            span: message,
            line: self.line,
        }
    }

    fn identifier(&mut self) -> Token<'t> {
        while is_alpha(self.peek()) || self.peek().is_ascii_digit() {
            self.advance();
        }
        self.make_token(self.identifier_type())
    }

    fn number(&mut self) -> Token<'t> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        // Look for a fractional part
        if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
            // Consume the '.'
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        self.make_token(TokenType::Number)
    }

    fn string(&mut self) -> Token<'t> {
        while self.peek() != b'"' && !self.is_at_end() {
            if self.peek() == b'\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            return self.error_token(b"Unterminated string");
        }
        self.advance();
        self.make_token(TokenType::String)
    }
}

impl<'a> Scanner<'a> {
    fn is_at_end(&self) -> bool {
        self.current == self.source.len() - 1 // Take '\0' into account
    }

    fn advance(&mut self) -> u8 {
        let res = self.peek();
        self.current += 1;
        res
    }

    fn peek(&self) -> u8 {
        self.source[self.current]
    }

    fn peek_next(&self) -> u8 {
        if self.is_at_end() {
            b'\0'
        } else {
            self.source[self.current + 1]
        }
    }

    fn match_token(&mut self, expected: u8) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek() != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();
            match c {
                b' ' | b'\r' | b'\t' => {
                    self.advance();
                }
                b'\n' => {
                    self.line += 1;
                    self.advance();
                }
                b'/' => {
                    if self.peek_next() == b'/' {
                        // A comment goes until the end of the line
                        while self.peek() != b'\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        break;
                    }
                }
                _ => {
                    break;
                }
            }
        }
    }

    fn check_keyword(
        &self,
        start: usize,
        length: usize,
        rest: &[u8],
        kind: TokenType,
    ) -> TokenType {
        if self.current - self.start == start + length
            && &self.source[self.start + start..][..length] == rest
        {
            kind
        } else {
            TokenType::Identifier
        }
    }

    fn identifier_type(&self) -> TokenType {
        match self.source[self.start] {
            b'a' => self.check_keyword(1, 2, b"nd", TokenType::And),
            b'c' => self.check_keyword(1, 4, b"lass", TokenType::Class),
            b'e' => self.check_keyword(1, 3, b"lse", TokenType::Else),
            b'f' => {
                if self.current - self.start > 1 {
                    match self.source[self.start + 1] {
                        b'a' => self.check_keyword(2, 3, b"lse", TokenType::False),
                        b'o' => self.check_keyword(2, 1, b"r", TokenType::For),
                        b'u' => self.check_keyword(2, 1, b"n", TokenType::Fun),
                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            b'i' => self.check_keyword(1, 1, b"f", TokenType::If),
            b'n' => self.check_keyword(1, 2, b"il", TokenType::Nil),
            b'o' => self.check_keyword(1, 1, b"r", TokenType::Or),
            b'p' => self.check_keyword(1, 4, b"rint", TokenType::Print),
            b'r' => self.check_keyword(1, 5, b"eturn", TokenType::Return),
            b's' => self.check_keyword(1, 4, b"uper", TokenType::Super),
            b't' => {
                if self.current - self.start > 1 {
                    match self.source[self.start + 1] {
                        b'h' => self.check_keyword(2, 2, b"is", TokenType::This),
                        b'r' => self.check_keyword(2, 2, b"ue", TokenType::True),
                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            b'v' => self.check_keyword(1, 2, b"ar", TokenType::Var),
            b'w' => self.check_keyword(1, 4, b"hile", TokenType::While),
            _ => TokenType::Identifier,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenType,
    pub span: &'a [u8],
    pub line: i32,
}

impl<'a> Token<'a> {
    pub fn identifier_equal(&self, other: &Self) -> bool {
        self.span == other.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen = 0,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    SemiColon,
    Slash,
    Star,
    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals
    Identifier,
    String,
    Number,
    // Keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    // Others
    Error,
    Eof,
}

fn is_alpha(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_'
}

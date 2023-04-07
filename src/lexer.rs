use std::{iter, str};

#[derive(Debug)]
pub enum Literal {
    Int,
    Float,
    Char,
}

#[derive(Debug)]
pub enum Symbol {
    // operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Tilde,

    // bitwise
    Shl,
    Shr,
    And,
    Or,

    // logical
    Not,
    AndAnd,
    OrOr,

    // relational
    Gt,
    Ge,
    Lt,
    Le,
    GtEq,
    LtEq,
    EqEq,
    Ne,

    // assignment
    Eq,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    PercentEq,
    CaretEq,
    ShlEq,
    ShrEq,
    AndEq,
    OrEq,

    //misc
    Colon,
    Dot,
    Hash,
}

#[derive(Debug)]
pub enum Keyword {
    Let,
    Fn,

    // conditionals
    If,
    Else,
    Elseif,

    // loops
    While,
    Do,
    For,

    // primitives
    Int,
    Float,
    Char,
}

#[derive(Debug)]
pub enum Delimiter {
    BraceOpen,
    BraceClose,
    ParenOpen,
    ParenClose,
}

#[derive(Debug)]
pub enum TokenKind {
    Literal(Literal),
    Symbol(Symbol),
    Keyword(Keyword),
    Delimiter(Delimiter),
    Newline,
    Identifier,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub value: Option<&'a str>,
    pub line: usize,
}

pub struct Lexer<'a> {
    file: &'a str,
    text: &'a str,
    chars: iter::Peekable<str::Chars<'a>>,
    line: usize,
    start: usize,
    end: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(file: &'a str, contents: &'a str) -> Lexer<'a> {
        Lexer {
            file,
            text: contents,
            chars: contents.chars().peekable(),
            line: 1,
            start: 0,
            end: 0,
        }
    }

    #[inline]
    fn error(&self) {
        eprintln!("error lexing \"{}:{}:{}\"", self.file, self.line, self.end);
    }

    fn next(&mut self) -> Option<char> {
        self.end += 1;
        self.chars.next()
    }

    fn skip(&mut self, c: char) {
        if self.next() != Some(c) {
            self.error();
            panic!("expected {}", c);
        }
    }

    fn get_numeric(&mut self) -> Token<'a> {
        let mut is_float: bool = false;
        while let Some(c) = self.chars.peek() {
            match c {
                '0'..='9' => {}
                '.' => {
                    if is_float {
                        self.error();
                        panic!("multiple decimals encountered")
                    }
                    is_float = true;
                }
                _ => break,
            }
            self.next();
        }

        Token {
            kind: if is_float {
                TokenKind::Literal(Literal::Float)
            } else {
                TokenKind::Literal(Literal::Int)
            },
            value: Some(&self.text[self.start..self.end]),
            line: self.line,
        }
    }

    fn get_char(&mut self) -> Token<'a> {
        self.skip('\'');

        if matches!(self.next(), Some('\'') | None) {
            self.error();
            panic!("A character literal cannot be empty");
        }

        self.skip('\'');

        Token {
            kind: TokenKind::Literal(Literal::Char),
            value: Some(&self.text[self.start + 1..self.end - 1]),
            line: self.line,
        }
    }

    fn get_delimiter(&mut self) -> Token<'a> {
        macro_rules! token_delimiter {
            ($a:expr) => {
                Token {
                    kind: TokenKind::Delimiter($a),
                    value: None,
                    line: self.line,
                }
            };
        }

        use Delimiter::*;

        match self.next() {
            Some(c) => match c {
                '{' => token_delimiter!(BraceOpen),
                '}' => token_delimiter!(BraceClose),
                '(' => token_delimiter!(ParenOpen),
                ')' => token_delimiter!(ParenClose),
                _ => {
                    self.error();
                    panic!("expected delimiter");
                }
            },
            None => {
                self.error();
                panic!("expected delimiter");
            }
        }
    }

    fn get_symbol(&mut self) -> Token<'a> {
        // handle ~, ., :
        macro_rules! token_symbol {
            ($a:expr) => {
                Token {
                    kind: TokenKind::Symbol($a),
                    value: None,
                    line: self.line,
                }
            };
        }

        // handle +, +=, -, -=, *, *=, /, /=, %, %=, ^, ^=, !, !=
        macro_rules! token_symbol_eq {
            ($a:expr, $b:expr) => {
                if self.chars.peek() == Some(&'=') {
                    self.next();
                    token_symbol!($b)
                } else {
                    token_symbol!($a)
                }
            };
        }

        // handle &, |, ||, &&, &=, |=
        macro_rules! token_symbol_logical {
            ($a:expr, $b:expr, $c:expr, $d:expr) => {
                match self.chars.peek() {
                    Some('=') => {
                        self.next();
                        token_symbol!($c)
                    }
                    Some($d) => {
                        self.next();
                        token_symbol!($b)
                    }
                    _ => token_symbol!($a),
                }
            };
        }

        // handle <, <=, >, >=, <<, >>, <<=, >>=
        macro_rules! token_symbol_compare {
            ($a:expr, $b:expr, $c:expr, $d:expr, $e:expr) => {
                match self.chars.peek() {
                    Some('=') => {
                        self.next();
                        token_symbol!($d)
                    }
                    Some($e) => {
                        self.next();
                        token_symbol_eq!($b, $c)
                    }
                    _ => token_symbol!($a),
                }
            };
        }

        use Symbol::*;

        match self.next() {
            Some(c) => match c {
                '+' => token_symbol_eq!(Plus, PlusEq),
                '-' => token_symbol_eq!(Minus, MinusEq),
                '*' => token_symbol_eq!(Star, StarEq),
                '/' => token_symbol_eq!(Slash, SlashEq),
                '%' => token_symbol_eq!(Percent, PercentEq),
                '^' => token_symbol_eq!(Caret, CaretEq),
                '!' => token_symbol_eq!(Not, Ne),
                '=' => token_symbol_eq!(Eq, EqEq),
                '&' => token_symbol_logical!(And, AndAnd, AndEq, '&'),
                '|' => token_symbol_logical!(Or, OrOr, OrEq, '|'),
                '<' => token_symbol_compare!(Lt, Shl, ShlEq, LtEq, '<'),
                '>' => token_symbol_compare!(Gt, Shr, ShrEq, GtEq, '>'),
                '~' => token_symbol!(Tilde),
                ':' => token_symbol!(Colon),
                '.' => token_symbol!(Dot),
                '#' => token_symbol!(Hash),
                _ => {
                    self.error();
                    panic!("expected symbol");
                }
            },
            None => {
                self.error();
                panic!("expected symbol");
            }
        }
    }

    fn get_alphanumeric(&mut self) -> Token<'a> {
        while let Some(c) = self.chars.peek() {
            match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' => {}
                _ => break,
            }
            self.next();
        }

        macro_rules! token_keyword {
            ($a:expr) => {
                Token {
                    kind: TokenKind::Keyword($a),
                    value: None,
                    line: self.line,
                }
            };
        }

        use Keyword::*;

        match &self.text[self.start..self.end] {
            "let" => token_keyword!(Let),
            "fn" => token_keyword!(Fn),
            "if" => token_keyword!(If),
            "else" => token_keyword!(Else),
            "elseif" => token_keyword!(Elseif),
            "while" => token_keyword!(While),
            "do" => token_keyword!(Do),
            "for" => token_keyword!(For),
            "int" => token_keyword!(Int),
            "float" => token_keyword!(Float),
            "char" => token_keyword!(Char),
            _ => Token {
                kind: TokenKind::Identifier,
                value: Some(&self.text[self.start..self.end]),
                line: self.line,
            },
        }
    }

    pub fn lex(&mut self) -> Vec<Token<'a>> {
        let mut tokens: Vec<Token> = Vec::new();

        while let Some(c) = self.chars.peek() {
            match c {
                ' ' | '\r' | '\t' => {
                    self.next();
                }
                '\n' => {
                    tokens.push(Token {
                        kind: TokenKind::Newline,
                        value: None,
                        line: self.line,
                    });
                    self.next();
                    self.line += 1;
                }
                '0'..='9' => tokens.push(self.get_numeric()),
                '\'' => tokens.push(self.get_char()),
                '{' | '}' | '(' | ')' => tokens.push(self.get_delimiter()),
                '+' | '-' | '*' | '/' | '%' | '^' | '~' | '&' | '|' | '!' | '<' | '>' | '='
                | ':' | '.' | '#' => tokens.push(self.get_symbol()),
                'a'..='z' | 'A'..='Z' => tokens.push(self.get_alphanumeric()),
                _ => {
                    self.error();
                    panic!("unknown character encountered");
                }
            }

            self.start = self.end;
        }

        tokens
    }
}

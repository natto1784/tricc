use std::{iter, str};

#[derive(Debug)]
pub enum TokenLiteral {
    Int,
    Float,
    Char,
}

#[derive(Debug)]
pub enum TokenSymbol {
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
pub enum TokenKeyword {
    Let,
    Fn,
    Ret,

    // conditionals
    If,
    Else,
    Elif,

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
pub enum TokenDelimiter {
    BraceOpen,
    BraceClose,
    ParenOpen,
    ParenClose,
}

#[derive(Debug)]
pub enum Token<'a> {
    Newline,
    Literal(TokenLiteral, &'a str),
    Symbol(TokenSymbol),
    Keyword(TokenKeyword),
    Delimiter(TokenDelimiter),
    Identifier(&'a str),
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

    fn escape_newline(&mut self) {
        while let Some(c) = self.chars.peek() {
            match c {
                '\r' | '\t' | ' ' => {
                    self.next();
                }
                '\n' => {
                    self.next();
                    break;
                }
                _ => {
                    self.error();
                    panic!("expected newline");
                },
            }
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

        Token::Literal(
            if is_float {
                TokenLiteral::Float
            } else {
                TokenLiteral::Int
            },
            &self.text[self.start..self.end],
        )
    }

    fn get_char(&mut self) -> Token<'a> {
        self.skip('\'');

        if matches!(self.next(), Some('\'') | None) {
            self.error();
            panic!("A character literal cannot be empty");
        }

        self.skip('\'');

        Token::Literal(TokenLiteral::Char, &self.text[self.start + 1..self.end - 1])
    }

    fn get_delimiter(&mut self) -> Token<'a> {
        use Token::Delimiter;
        use TokenDelimiter::*;

        match self.next() {
            Some(c) => match c {
                '{' => Delimiter(BraceOpen),
                '}' => Delimiter(BraceClose),
                '(' => Delimiter(ParenOpen),
                ')' => Delimiter(ParenClose),
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
        use Token::Symbol;

        // handle +, +=, -, -=, *, *=, /, /=, %, %=, ^, ^=, !, !=
        macro_rules! token_symbol_eq {
            ($a:expr, $b:expr) => {
                if self.chars.peek() == Some(&'=') {
                    self.next();
                    Symbol($b)
                } else {
                    Symbol($a)
                }
            };
        }

        // handle &, |, ||, &&, &=, |=
        macro_rules! token_symbol_logical {
            ($a:expr, $b:expr, $c:expr, $d:expr) => {
                match self.chars.peek() {
                    Some('=') => {
                        self.next();
                        Symbol($c)
                    }
                    Some($d) => {
                        self.next();
                        Symbol($b)
                    }
                    _ => Symbol($a),
                }
            };
        }

        // handle <, <=, >, >=, <<, >>, <<=, >>=
        macro_rules! token_symbol_compare {
            ($a:expr, $b:expr, $c:expr, $d:expr, $e:expr) => {
                match self.chars.peek() {
                    Some('=') => {
                        self.next();
                        Symbol($d)
                    }
                    Some($e) => {
                        self.next();
                        token_symbol_eq!($b, $c)
                    }
                    _ => Symbol($a),
                }
            };
        }

        use TokenSymbol::*;

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
                '~' => Symbol(Tilde),
                ':' => Symbol(Colon),
                '.' => Symbol(Dot),
                '#' => Symbol(Hash),
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

        use Token::Keyword;
        use TokenKeyword::*;

        match &self.text[self.start..self.end] {
            "let" => Keyword(Let),
            "fn" => Keyword(Fn),
            "ret" => Keyword(Ret),
            "if" => Keyword(If),
            "else" => Keyword(Else),
            "elif" => Keyword(Elif),
            "while" => Keyword(While),
            "do" => Keyword(Do),
            "for" => Keyword(For),
            "int" => Keyword(Int),
            "float" => Keyword(Float),
            "char" => Keyword(Char),
            _ => Token::Identifier(&self.text[self.start..self.end]),
        }
    }

    pub fn lex(&mut self) -> Vec<Token<'a>> {
        let mut tokens: Vec<Token> = Vec::new();

        while let Some(c) = self.chars.peek() {
            match c {
                ' ' | '\r' | '\t' => {
                    self.next();
                }
                '\\' => {
                    self.next();
                    self.escape_newline();
                }
                '\n' => {
                    tokens.push(Token::Newline);
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

use std::collections::VecDeque;
use std::iter::Peekable;
use std::rc::Rc;
use std::str;

/// All token literals
///
/// TODO: Add string
#[derive(Debug, PartialEq)]
pub enum TokenLiteral {
    Int,
    Float,
    Char,
}

/// All token symbols
#[derive(Debug, PartialEq)]
pub enum TokenSymbol {
    // arithmetic
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
    Comma,
    Hash,
}

/// All token keywod
#[derive(Debug, PartialEq)]
pub enum TokenKeyword {
    // parents
    Fn,
    Class,
    Module,

    // statements
    Static,
    Let,
    Ret,

    // conditionals
    If,
    Else,
    Elif,

    // control flow
    Loop,
    Break,
    Continue,

    // primitives
    Int,
    Float,
    Char,
}

/// All token delimiters
///
/// TODO: Maybe add \[ and \]
#[derive(Debug, PartialEq)]
pub enum TokenDelimiter {
    BraceOpen,
    BraceClose,
    ParenOpen,
    ParenClose,
}

/// All tokens
#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Newline,
    Eof,
    Literal(TokenLiteral),
    Symbol(TokenSymbol),
    Keyword(TokenKeyword),
    Delimiter(TokenDelimiter),
    Identifier,
    Invalid,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    /// Holds the reference to the tokenized string
    ///
    /// For example, if `kind` is of type [`TokenKind::Identifier`], this would contain the value
    /// of that identifier
    pub val: Rc<str>,
}

pub struct Lexer<'a> {
    /// The entire text to be tokenized
    text: &'a str,
    /// A peekable iterate for `text`
    chars: Peekable<str::Chars<'a>>,
    /// A peekable double ended queue for the tokens
    tokens: VecDeque<Token>,
    /// Current line number
    pub line: usize,
    pub col: usize,
    /// Start character index for the current token
    start: usize,
    /// End character index for the current token
    end: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new [`Lexer`] instance with the provided content.
    ///
    /// The `Lexer` is responsible for tokenizing the given text, making it easier to
    /// perform various parsing operations.
    ///
    /// # Arguments
    ///
    /// * `content`: The text to tokenize.
    ///
    /// # Returns
    ///
    /// A new instance of `Lexer` initialized with the provided `content`.
    ///
    /// # Example
    ///
    /// ```
    /// use tricc::lexer::Lexer;
    ///
    /// let lexer = Lexer::new("let example: int = 4");
    /// ```
    pub fn new(content: &'a str) -> Self {
        Lexer {
            text: content,
            chars: content.chars().peekable(),
            tokens: VecDeque::new(),
            line: 1,
            col: 1,
            start: 0,
            end: 0,
        }
    }

    #[inline]
    fn new_token(&self, kind: TokenKind) -> Token {
        Token {
            kind,
            val: Rc::from(&self.text[self.start..self.end]),
        }
    }

    #[inline]
    fn error(&self, msg: &str) {
        eprintln!("Lexer: {}, at \"{}:{}\"", msg, self.line, self.end);
    }

    #[inline]
    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    #[inline]
    fn next(&mut self) -> Option<char> {
        self.end += 1;
        self.col += 1;
        self.chars.next()
    }

    fn skip_whitespace(&mut self) {
        let mut ignore_nl: bool = false;

        while let Some(c) = self.peek() {
            match c {
                '\r' | '\t' | ' ' => {
                    self.next();
                }
                '\n' => {
                    if ignore_nl {
                        ignore_nl = false;
                        self.next();
                    } else {
                        break;
                    }
                }
                '\\' => {
                    self.next();
                    ignore_nl = true;
                }
                _ => break,
            }
        }
    }

    fn get_numeric(&mut self) -> Token {
        let mut is_float: bool = false;
        while let Some(c) = self.peek() {
            match c {
                '0'..='9' => {}
                '.' => {
                    if is_float {
                        self.error("multiple decimals encountered");
                        return self.new_token(TokenKind::Invalid);
                    }
                    is_float = true;
                }
                'e' | 'E' => {
                    self.next();
                    is_float = true;
                    break;
                }
                _ => break,
            }
            self.next();
        }

        self.new_token(TokenKind::Literal(if is_float {
            TokenLiteral::Float
        } else {
            TokenLiteral::Int
        }))
    }

    fn get_char(&mut self) -> Token {
        // skip '
        self.next();

        if matches!(self.next(), Some('\'') | None) {
            self.error("Expected character literal");
            return self.new_token(TokenKind::Invalid);
        }

        if self.peek() != Some(&'\'') {
            self.error("Expected '");
            return self.new_token(TokenKind::Invalid);
        }

        // skip '
        self.next();

        self.new_token(TokenKind::Literal(TokenLiteral::Char))
    }

    fn get_alphanumeric(&mut self) -> Token {
        while let Some(c) = self.peek() {
            match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' => {}
                _ => break,
            }
            self.next();
        }

        use TokenKeyword::*;
        use TokenKind::Keyword;

        self.new_token(match &self.text[self.start..self.end] {
            "fn" => Keyword(Fn),
            "class" => Keyword(Class),
            "module" => Keyword(Module),
            "static" => Keyword(Static),
            "let" => Keyword(Let),
            "ret" => Keyword(Ret),
            "if" => Keyword(If),
            "else" => Keyword(Else),
            "elif" => Keyword(Elif),
            "loop" => Keyword(Loop),
            "break" => Keyword(Break),
            "continue" => Keyword(Continue),
            "int" => Keyword(Int),
            "float" => Keyword(Float),
            "char" => Keyword(Char),
            _ => TokenKind::Identifier,
        })
    }

    fn get_symbol(&mut self) -> Token {
        let c = self.next().unwrap();

        use TokenDelimiter::*;
        use TokenKind::{
            Delimiter,
            Symbol,
        };
        use TokenSymbol::*;

        // handle +, +=, -, -=, *, *=, /, /=, %, %=, ^, ^=, !, !=
        macro_rules! token_symbol_eq {
            ($a:expr, $b:expr) => {
                match self.peek() {
                    Some('=') => {
                        self.next();
                        Symbol($b)
                    }
                    _ => Symbol($a),
                }
            };
        }

        // handle &, |, ||, &&, &=, |=
        macro_rules! token_symbol_logical {
            ($a:expr, $b:expr, $c:expr, $d:expr) => {
                match self.peek() {
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
                match self.peek() {
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

        let typ = match c {
            '{' => Delimiter(BraceOpen),
            '}' => Delimiter(BraceClose),
            '(' => Delimiter(ParenOpen),
            ')' => Delimiter(ParenClose),
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
            ',' => Symbol(Comma),
            '#' => Symbol(Hash),
            _ => {
                self.error("Unknown character encountered");
                TokenKind::Invalid
            }
        };
        self.new_token(typ)
    }

    fn lex(&mut self) {
        self.skip_whitespace();
        self.start = self.end;

        let token = if let Some(c) = self.peek() {
            match c {
                '\n' => {
                    self.next();
                    self.line += 1;
                    self.col = 0;
                    self.new_token(TokenKind::Newline)
                }
                '0'..='9' => self.get_numeric(),
                'a'..='z' | 'A'..='Z' => self.get_alphanumeric(),
                '\'' => self.get_char(),
                _ => self.get_symbol(),
            }
        } else {
            self.new_token(TokenKind::Eof)
        };
        self.tokens.push_back(token);
    }

    /// Peeks at the next token and returns a reference to it
    pub fn peek_token(&mut self) -> &Token {
        if self.tokens.is_empty() {
            self.lex();
        }
        &self.tokens[0]
    }

    /// Returns the next token, moving the lexer forward
    pub fn next_token(&mut self) -> Token {
        if self.tokens.is_empty() {
            self.lex();
        }
        self.tokens.pop_front().unwrap()
    }
}

#[test]
fn test_peek_next() {
    let mut lexer = Lexer::new("test01");
    assert_eq!(lexer.peek(), Some(&'t'));
    assert_eq!(lexer.next(), Some('t'));
    assert_eq!(lexer.peek(), Some(&'e'));
    assert_eq!(lexer.peek(), Some(&'e'));
    assert_eq!(lexer.next(), Some('e'));
    assert_eq!(lexer.next(), Some('s'));
    assert_eq!(lexer.next(), Some('t'));
    assert_eq!(lexer.next(), Some('0'));
    assert_eq!(lexer.peek(), Some(&'1'));
    assert_eq!(lexer.next(), Some('1'));
    assert_eq!(lexer.peek(), None);
    assert_eq!(lexer.next(), None);
    assert_eq!(lexer.peek(), None);
}

#[test]
fn test_tokens() {
    let mut lexer = Lexer::new("let test02 = 4 << 1");

    use TokenKind::*;

    assert_eq!(lexer.peek_token().kind, Keyword(TokenKeyword::Let));
    assert_eq!(lexer.next_token().kind, Keyword(TokenKeyword::Let));

    let mut token = lexer.next_token();
    assert_eq!(token.kind, Identifier);
    assert_eq!(*token.val, *"test02");

    assert_eq!(lexer.next_token().kind, Symbol(TokenSymbol::Eq));

    token = lexer.next_token();
    assert_eq!(token.kind, Literal(TokenLiteral::Int));
    assert_eq!(*token.val, *"4");

    assert_eq!(lexer.next_token().kind, Symbol(TokenSymbol::Shl));

    assert_eq!(lexer.peek_token().kind, Literal(TokenLiteral::Int));
    assert_eq!(*lexer.peek_token().val, *"1");
    token = lexer.next_token();
    assert_eq!(token.kind, Literal(TokenLiteral::Int));
    assert_eq!(*token.val, *"1");

    assert_eq!(lexer.peek_token().kind, Eof);
    assert_eq!(lexer.next_token().kind, Eof);
    assert_eq!(lexer.peek_token().kind, Eof);
    assert_eq!(lexer.next_token().kind, Eof);
}

#[test]
fn test_tokens_2() {
    let mut lexer = Lexer::new("let test03: char = 'h'");

    use TokenKind::*;

    assert_eq!(lexer.peek_token().kind, Keyword(TokenKeyword::Let));
    assert_eq!(lexer.next_token().kind, Keyword(TokenKeyword::Let));

    let mut token = lexer.next_token();
    assert_eq!(token.kind, Identifier);
    assert_eq!(*token.val, *"test03");

    assert_eq!(lexer.next_token().kind, Symbol(TokenSymbol::Colon));
    assert_eq!(lexer.next_token().kind, Keyword(TokenKeyword::Char));
    assert_eq!(lexer.next_token().kind, Symbol(TokenSymbol::Eq));

    assert_eq!(lexer.peek_token().kind, Literal(TokenLiteral::Char));
    assert_eq!(*lexer.peek_token().val, *"'h'");
    token = lexer.next_token();
    assert_eq!(token.kind, Literal(TokenLiteral::Char));
    assert_eq!(*token.val, *"'h'");

    assert_eq!(lexer.peek_token().kind, Eof);
    assert_eq!(lexer.next_token().kind, Eof);
}

#[test]
fn test_tokens_eof() {
    let mut lexer = Lexer::new("");

    assert_eq!(lexer.peek_token().kind, TokenKind::Eof);
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_tokens_numeric() {
    let mut lexer = Lexer::new("3342");

    let token = lexer.next_token();
    assert_eq!(token.kind, TokenKind::Literal(TokenLiteral::Int));
    assert_eq!(*token.val, *"3342");

    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_tokens_numeric_2() {
    let mut lexer = Lexer::new("334.2e");

    let token = lexer.next_token();
    assert_eq!(token.kind, TokenKind::Literal(TokenLiteral::Float));
    assert_eq!(*token.val, *"334.2e");

    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_tokens_numeric_3() {
    let mut lexer = Lexer::new("334.2e-5");

    let mut token = lexer.next_token();
    assert_eq!(token.kind, TokenKind::Literal(TokenLiteral::Float));
    assert_eq!(*token.val, *"334.2e");

    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Symbol(TokenSymbol::Minus)
    );

    token = lexer.next_token();
    assert_eq!(token.kind, TokenKind::Literal(TokenLiteral::Int));
    assert_eq!(*token.val, *"5");

    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

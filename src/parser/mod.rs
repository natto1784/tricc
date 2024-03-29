//! A naive parser just to get started
//!
//! Can only parse module, class and function declaration now along with let statements

mod entity;
mod expr;
mod literal;
mod statement;

use crate::ast::{
    Parent,
    Ty,
};
use crate::lexer::{
    Lexer,
    Token,
    TokenKeyword,
    TokenKind,
    TokenSymbol,
};
use std::rc::Rc;

pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    /// Creates a new [`Parser`] instance.
    pub fn new(contents: &'a str) -> Parser<'a> {
        Parser {
            lexer: Lexer::new(contents),
        }
    }

    #[inline]
    fn error(&self, message: &str) {
        eprintln!(
            "Parser: {}, at \"{}:{}\"",
            message, self.lexer.line, self.lexer.col
        );
    }

    #[inline]
    fn error_expected(&self, expected: &str, found: &str) {
        self.error(&format!("expected {}, found {}", expected, found));
    }

    #[inline]
    fn error_expected_peek(&mut self, expected: &str) {
        let found = &Rc::clone(&self.peek_token().val);
        self.error_expected(expected, found);
    }

    #[inline]
    fn next_token(&mut self) -> Token {
        let t = self.lexer.next_token();
        println!("{:?}", t);
        t
    }

    #[inline]
    fn peek_token(&mut self) -> &Token {
        return self.lexer.peek_token();
    }

    /// newline ::= "}\n"
    fn trim_newlines(&mut self) {
        while self.peek_token().kind == TokenKind::Newline {
            self.next_token();
        }
    }

    fn skip_token(&mut self, kind: TokenKind) -> bool {
        if self.peek_token().kind == kind {
            self.next_token();
            return true;
        }

        false
    }

    fn check_newline_or_tok(&mut self, token: TokenKind) -> bool {
        match self.peek_token().kind {
            TokenKind::Newline => true,
            d if d == token => true,
            _ => false,
        }
    }

    /// ty ::= "int" | "float" | "char"
    fn parse_ty(&mut self) -> Option<Ty> {
        let ty: Ty;

        if let TokenKind::Keyword(keyword) = &self.peek_token().kind {
            ty = match keyword {
                TokenKeyword::Int => Ty::Int,
                TokenKeyword::Char => Ty::Char,
                TokenKeyword::Float => Ty::Float,
                _ => {
                    self.error_expected_peek("ty");
                    return None;
                }
            };
        } else {
            self.error_expected_peek("ty");
            return None;
        }
        self.next_token();
        Some(ty)
    }

    /// ident ::= ( letter | "_" ) { letter | digit | "_" }
    fn parse_ident(&mut self) -> Option<Rc<str>> {
        if self.peek_token().kind != TokenKind::Identifier {
            self.error_expected_peek("identifier");
            return None;
        }

        Some(Rc::clone(&self.next_token().val))
    }

    /// identWithTy ::= letter ":" ty
    fn parse_ident_with_ty(&mut self) -> Option<(Rc<str>, Ty)> {
        let ident = self.parse_ident()?;

        if !self.skip_token(TokenKind::Symbol(TokenSymbol::Colon)) {
            self.error_expected_peek(":");
            return None;
        }

        Some((ident, self.parse_ty()?))
    }

    /// Returns an [`Entity`] vector after parsing
    ///
    /// parent ::= { entity }
    /// [`Entity`]: crate::ast::Entity
    pub fn parse(&mut self) -> Option<Parent> {
        let mut parent = vec![];

        loop {
            match self.peek_token().kind {
                TokenKind::Newline => self.trim_newlines(),
                TokenKind::Eof => break,
                _ => {
                    parent.push(self.parse_entity()?);
                    if !self.check_newline_or_tok(TokenKind::Eof) {
                        self.error_expected_peek("newline or end of file");
                        return None;
                    }
                }
            }
        }
        Some(parent)
    }
}

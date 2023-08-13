use super::Parser;
use crate::ast::*;
use crate::lexer::{
    TokenDelimiter,
    TokenKeyword,
    TokenKind,
    TokenSymbol,
};
use std::rc::Rc;

impl<'a> Parser<'a> {
    /// entity ::= module | class | fn | static
    pub(super) fn parse_entity(&mut self) -> Option<Entity> {
        use TokenKeyword::*;
        let token = self.peek_token();

        if let TokenKind::Keyword(keyword) = &token.kind {
            Some(match keyword {
                Module => Entity::Module(self.parse_module()?),
                Class => Entity::Class(self.parse_class()?),
                Fn => Entity::Fn(self.parse_fn()?),
                Static => Entity::Static(self.parse_static()?),
                _ => {
                    self.error_expected_peek("entity");
                    return None;
                }
            })
        } else {
            self.error_expected_peek("entity");
            None
        }
    }

    /// module ::= "module" ident "{" { module | fn | static | class } "}"
    fn parse_module(&mut self) -> Option<Module> {
        self.next_token();

        let name = self.parse_ident()?;
        let mut children = vec![];

        if !self.skip_token(TokenKind::Delimiter(TokenDelimiter::BraceOpen)) {
            self.error_expected_peek("{");
            return None;
        }

        loop {
            use TokenKeyword::*;
            self.trim_newlines();
            if let TokenKind::Keyword(keyword) = &self.peek_token().kind {
                children.push(match keyword {
                    Module => ModuleChildren::Module(self.parse_module()?),
                    Fn => ModuleChildren::Fn(self.parse_fn()?),
                    Static => ModuleChildren::Static(self.parse_static()?),
                    Class => ModuleChildren::Class(self.parse_class()?),
                    _ => {
                        self.error_expected_peek("module child");
                        return None;
                    }
                });
            } else if !self.skip_token(TokenKind::Delimiter(TokenDelimiter::BraceClose)) {
                self.error_expected_peek("}");
                return None;
            } else {
                break;
            }
        }

        Some(Module { name, children })
    }

    /// class ::= "class" ident "{" { fn | static | let } "}"
    fn parse_class(&mut self) -> Option<Class> {
        self.next_token();

        let name = self.parse_ident()?;
        let mut children = vec![];

        if !self.skip_token(TokenKind::Delimiter(TokenDelimiter::BraceOpen)) {
            self.error_expected_peek("{");
            return None;
        }

        loop {
            use TokenKeyword::*;
            self.trim_newlines();
            if let TokenKind::Keyword(keyword) = &self.peek_token().kind {
                children.push(match keyword {
                    Fn => ClassChildren::Fn(self.parse_fn()?),
                    Static => ClassChildren::Static(self.parse_static()?),
                    Let => ClassChildren::Let(self.parse_let()?),
                    _ => {
                        self.error_expected_peek("class child");
                        return None;
                    }
                });
            } else if !self.skip_token(TokenKind::Delimiter(TokenDelimiter::BraceClose)) {
                self.error_expected_peek("}");
                return None;
            } else {
                break;
            }
        }

        Some(Class { name, children })
    }

    /// fn ::= "fn" ident "(" [ identWithTy { "," identWithTy } ] ")" [ ":" ty ]
    ///        "{" { statement } "}"
    fn parse_fn(&mut self) -> Option<Fn> {
        self.next_token();

        let name = self.parse_ident()?;
        let mut params: Vec<(Rc<str>, Ty)> = vec![];
        let mut return_typ: Option<Ty> = None;
        let mut children: Vec<Statement> = vec![];

        if !self.skip_token(TokenKind::Delimiter(TokenDelimiter::ParenOpen)) {
            self.error_expected_peek("(");
            return None;
        }

        loop {
            if self.peek_token().kind == TokenKind::Identifier {
                params.push(self.parse_ident_with_ty()?);
            }

            if !self.skip_token(TokenKind::Symbol(TokenSymbol::Comma)) {
                if !self.skip_token(TokenKind::Delimiter(TokenDelimiter::ParenClose)) {
                    self.error_expected_peek(", or )");
                    return None;
                } else {
                    break;
                }
            }
        }

        if self.skip_token(TokenKind::Symbol(TokenSymbol::Colon)) {
            return_typ = Some(self.parse_ty()?);
        }

        if !self.skip_token(TokenKind::Delimiter(TokenDelimiter::BraceOpen)) {
            self.error_expected_peek("{");
            return None;
        }

        loop {
            self.trim_newlines();
            if self.skip_token(TokenKind::Delimiter(TokenDelimiter::BraceClose)) {
                break;
            }
            children.push(self.parse_statement()?)
        }

        Some(Fn {
            name,
            return_ty: return_typ,
            params,
            children,
        })
    }
}

#[test]
fn test_parse_entity() {
    let mut parser = Parser::new(
        r#"module module01 {
             class class01 {
               fn fn01(param01: char, param02: float) {
                 static let let01: int = 4
               }
             }

             fn fn02 (): int { }
          }"#,
    );
    assert_eq!(
        parser.parse_entity(),
        Some(Entity::Module(Module {
            name: "module01".into(),
            children: vec![
                ModuleChildren::Class(Class {
                    name: "class01".into(),
                    children: vec![ClassChildren::Fn(Fn {
                        name: "fn01".into(),
                        return_ty: None,
                        params: vec![("param01".into(), Ty::Char), ("param02".into(), Ty::Float)],
                        children: vec![Statement::Static(Let {
                            name: "let01".into(),
                            ty: Ty::Int,
                            expr: Some(Expr::Literal(Literal::Int(4)))
                        })]
                    })]
                }),
                ModuleChildren::Fn(Fn {
                    name: "fn02".into(),
                    return_ty: Some(Ty::Int),
                    params: vec![],
                    children: vec![]
                })
            ]
        }))
    );
}

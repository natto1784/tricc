use super::Parser;
use crate::ast::*;
use crate::lexer::{
    TokenKeyword,
    TokenKind,
    TokenLiteral,
    TokenSymbol,
};

impl<'a> Parser<'a> {
    pub(super) fn parse_statement(&mut self) -> Option<Statement> {
        use TokenKeyword::*;

        match self.peek_token().kind {
            TokenKind::Keyword(Static) => Some(Statement::Static(self.parse_static()?)),
            TokenKind::Keyword(Let) => Some(Statement::Let(self.parse_let()?)),
            _ => Some(Statement::Expr(self.parse_expr()?)),
        }
    }

    pub(super) fn parse_static(&mut self) -> Option<Let> {
        self.next_token();
        if self.peek_token().kind != TokenKind::Keyword(TokenKeyword::Let) {
            self.error_expected_peek("let");
            return None;
        }

        self.parse_let()
    }

    pub(super) fn parse_let(&mut self) -> Option<Let> {
        self.next_token();

        let (name, ty) = self.parse_ident_with_type()?;
        let expr;

        if self.skip_token(TokenKind::Symbol(TokenSymbol::Eq)) {
            expr = Some(self.parse_expr()?);
            self.trim_newlines();
        } else if self.skip_token(TokenKind::Newline) {
            expr = None;
        } else {
            self.error_expected_peek("= or newline");
            return None;
        }

        Some(Let { name, ty, expr })
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        use TokenKind::*;

        match self.peek_token().kind {
            Literal(TokenLiteral::Int) => Some(Expr::Int(self.parse_int()?)),
            Literal(TokenLiteral::Float) => Some(Expr::Float(self.parse_float()?)),
            Literal(TokenLiteral::Char) => Some(Expr::Char(self.parse_char()?)),
            _ => {
                self.error_expected_peek("expression");
                None
            }
        }
    }
}

#[test]
fn test_parse_let() {
    let mut parser = Parser::new(
        r#"static let test01: int = 4
           let test02: char = '6'
           static let test03: float
           let test04 = 9"#,
    );
    assert_eq!(
        parser.parse_static(),
        Some(Let {
            name: "test01".into(),
            ty: Ty::Int,
            expr: Some(Expr::Int(4))
        })
    );
    assert_eq!(
        parser.parse_let(),
        Some(Let {
            name: "test02".into(),
            ty: Ty::Char,
            expr: Some(Expr::Char('6'))
        })
    );
    assert_eq!(
        parser.parse_static(),
        Some(Let {
            name: "test03".into(),
            ty: Ty::Float,
            expr: None
        })
    );
    assert_eq!(parser.parse_let(), None);
}

#[test]
fn test_parse_expr_literals() {
    let mut parser = Parser::new("4524 3123.15e4 9e2 9083482.429455 'c' 3331.13.3");
    assert_eq!(parser.parse_expr(), Some(Expr::Int(4524)));
    assert_eq!(parser.parse_expr(), Some(Expr::Float(3123.15e4)));
    assert_eq!(parser.parse_expr(), Some(Expr::Float(9e2)));
    assert_eq!(parser.parse_expr(), Some(Expr::Float(9083482.429455)));
    assert_eq!(parser.parse_expr(), Some(Expr::Char('c')));
    assert_eq!(parser.parse_expr(), None);
}

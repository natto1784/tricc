use super::Parser;
use crate::ast::*;
use crate::lexer::{
    TokenKeyword,
    TokenKind,
    TokenSymbol,
};

impl<'a> Parser<'a> {
    /// statement ::= static | let | expr
    pub(super) fn parse_statement(&mut self) -> Option<Statement> {
        use TokenKeyword::*;

        Some(match self.peek_token().kind {
            TokenKind::Keyword(Static) => Statement::Static(self.parse_static()?),
            TokenKind::Keyword(Let) => Statement::Let(self.parse_let()?),
            _ => Statement::Expr(self.parse_expr_ln()?),
        })
    }

    /// static ::="static" let
    pub(super) fn parse_static(&mut self) -> Option<Let> {
        self.next_token();

        if self.peek_token().kind != TokenKind::Keyword(TokenKeyword::Let) {
            self.error_expected_peek("let");
            return None;
        }

        self.parse_let()
    }

    /// let ::= "let" identWithTy "=" expr
    pub(super) fn parse_let(&mut self) -> Option<Let> {
        self.next_token();

        let (name, ty) = self.parse_ident_with_ty()?;

        let expr = if self.skip_token(TokenKind::Symbol(TokenSymbol::Eq)) {
            self.parse_expr_ln()
        } else if self.skip_token(TokenKind::Newline) {
            None
        } else {
            self.error_expected_peek("= or newline");
            return None;
        };

        Some(Let { name, ty, expr })
    }
}

#[test]
fn test_parse_let() {
    use Literal::*;

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
            expr: Some(Expr::Literal(Int(4)))
        })
    );
    assert_eq!(
        parser.parse_let(),
        Some(Let {
            name: "test02".into(),
            ty: Ty::Char,
            expr: Some(Expr::Literal(Char('6')))
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

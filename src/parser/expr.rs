use super::Parser;
use crate::ast::{
    self,
    *,
};
use crate::lexer::{
    TokenDelimiter,
    TokenKeyword,
    TokenKind,
    TokenLiteral,
    TokenSymbol,
};
use std::rc::Rc;

impl<'a> Parser<'a> {
    /// exprIf ::= "if" expr block [ else (block | exprIf ) ]
    fn parse_expr_if(&mut self) -> Option<If> {
        // skip "if"
        self.next_token();

        let cond = Box::new(self.parse_expr()?);
        let then = self.parse_expr_block()?;

        if !self.skip_token(TokenKind::Keyword(TokenKeyword::Else)) {
            return Some(If {
                cond,
                then,
                or: None,
            });
        }

        if self.peek_token().kind != TokenKind::Keyword(TokenKeyword::If) {
            return Some(If {
                cond,
                then,
                or: Some(Box::new(ElseType::Else(self.parse_expr_block()?))),
            });
        }

        Some(If {
            cond,
            then,
            or: Some(Box::new(ElseType::If(self.parse_expr_if()?))),
        })
    }

    /// exprBlock ::= "{" { statement } "}"
    fn parse_expr_block(&mut self) -> Option<Vec<Statement>> {
        let mut statements = vec![];

        // skip {
        self.next_token();

        loop {
            self.trim_newlines();
            if self.skip_token(TokenKind::Delimiter(TokenDelimiter::BraceClose)) {
                break;
            }
            statements.push(self.parse_statement()?);
            if !self.check_newline_or_tok(TokenKind::Delimiter(TokenDelimiter::BraceClose)) {
                self.error_expected_peek("newline or }");
                return None;
            }
        }

        Some(statements)
    }

    /// exprLoop ::= "loop" exprBlock
    fn parse_expr_loop(&mut self) -> Option<Vec<Statement>> {
        self.next_token();
        if self.peek_token().kind != TokenKind::Delimiter(TokenDelimiter::BraceOpen) {
            self.error_expected_peek("{");
            return None;
        }

        self.parse_expr_block()
    }

    /// exprAtom ::= ( "(" expr ")" ) | ident | int | float | char | exprBlock | exprLoop | exprIf
    fn parse_expr_atom(&mut self) -> Option<Expr> {
        use ast::Literal::*;
        use TokenKind::*;

        // TODO: check lvalue validity in the analysis phase
        Some(match self.peek_token().kind {
            Delimiter(TokenDelimiter::ParenOpen) => {
                self.next_token(); // skip (

                let expr = self.parse_expr()?;

                if !self.skip_token(TokenKind::Delimiter(TokenDelimiter::ParenClose)) {
                    self.error_expected_peek(")");
                    return None;
                }

                expr
            }
            Identifier => {
                let token = self.next_token();
                Expr::Identifier(Rc::clone(&token.val))
            }
            Literal(TokenLiteral::Int) => Expr::Literal(Int(self.parse_int()?)),
            Literal(TokenLiteral::Float) => Expr::Literal(Float(self.parse_float()?)),
            Literal(TokenLiteral::Char) => Expr::Literal(Char(self.parse_char()?)),
            Delimiter(TokenDelimiter::BraceOpen) => Expr::Block(self.parse_expr_block()?),
            Keyword(TokenKeyword::Loop) => Expr::Loop(self.parse_expr_loop()?),
            Keyword(TokenKeyword::If) => Expr::If(self.parse_expr_if()?),
            _ => {
                self.error_expected_peek("expression");
                return None;
            }
        })
    }

    /// exprUnary ::= [ unaryOp ] exprAtom
    /// unaryOp   ::= "+" | "-" | "~"
    fn parse_expr_unary(&mut self) -> Option<Expr> {
        use TokenSymbol::*;
        Some(match self.peek_token().kind {
            TokenKind::Symbol(symbol @ (Minus | Plus | Tilde)) => {
                self.next_token();
                Expr::Op(symbol, Box::new(self.parse_expr_atom()?), None)
            }
            _ => self.parse_expr_atom()?,
        })
    }

    /// exprArithmeticMul ::= exprUnary [ arithmeticMulOp exprArithmeticMul ]
    /// arithmeticMulOp   ::= "*" | "/" | "%"
    fn parse_expr_arithmetic_mul(&mut self) -> Option<Expr> {
        use TokenSymbol::*;
        let lhs = self.parse_expr_unary()?;

        Some(match self.peek_token().kind {
            TokenKind::Symbol(symbol @ (Star | Slash | Percent)) => {
                self.next_token();
                Expr::Op(
                    symbol,
                    Box::new(lhs),
                    Some(Box::new(self.parse_expr_arithmetic_mul()?)),
                )
            }
            _ => lhs,
        })
    }

    /// exprArithmeticAdd ::= exprArithmeticMul [ arithmeticAddOp exprArithmeticAdd ]
    /// arithmeticAddOp   ::= "+" | "-"
    fn parse_expr_arithmetic_add(&mut self) -> Option<Expr> {
        use TokenSymbol::*;
        let lhs = self.parse_expr_arithmetic_mul()?;

        Some(match self.peek_token().kind {
            TokenKind::Symbol(symbol @ (Plus | Minus)) => {
                self.next_token();
                Expr::Op(
                    symbol,
                    Box::new(lhs),
                    Some(Box::new(self.parse_expr_arithmetic_add()?)),
                )
            }
            _ => lhs,
        })
    }

    /// exprBitwiseShift ::= exprArithmeticAdd [ bitwiseShiftOp exprBitwiseShift ]
    /// bitwiseShiftOp   ::= "<<" | ">>"
    fn parse_expr_bitwise_shift(&mut self) -> Option<Expr> {
        use TokenSymbol::*;
        let lhs = self.parse_expr_arithmetic_add()?;

        Some(match self.peek_token().kind {
            TokenKind::Symbol(symbol @ (Shl | Shr)) => {
                self.next_token();
                Expr::Op(
                    symbol,
                    Box::new(lhs),
                    Some(Box::new(self.parse_expr_bitwise_shift()?)),
                )
            }
            _ => lhs,
        })
    }

    /// exprBitwiseAnd ::= exprBitwiseShift [ "&" exprBitwiseAnd ]
    fn parse_expr_bitwise_and(&mut self) -> Option<Expr> {
        let lhs = self.parse_expr_bitwise_shift()?;
        let symbol = TokenSymbol::And;

        if !self.skip_token(TokenKind::Symbol(symbol)) {
            return Some(lhs);
        }

        Some(Expr::Op(
            symbol,
            Box::new(lhs),
            Some(Box::new(self.parse_expr_bitwise_and()?)),
        ))
    }

    /// exprBitwiseXor ::= exprBitwiseAnd [ "^" exprBitwiseXor ]
    fn parse_expr_bitwise_xor(&mut self) -> Option<Expr> {
        let lhs = self.parse_expr_bitwise_and()?;
        let symbol = TokenSymbol::Caret;

        if !self.skip_token(TokenKind::Symbol(symbol)) {
            return Some(lhs);
        }

        Some(Expr::Op(
            symbol,
            Box::new(lhs),
            Some(Box::new(self.parse_expr_bitwise_xor()?)),
        ))
    }

    /// exprBiwiseOr ::= exprBitwiseXor [ "|" exprBitwiseOr ]
    fn parse_expr_bitwise_or(&mut self) -> Option<Expr> {
        let lhs = self.parse_expr_bitwise_xor()?;
        let symbol = TokenSymbol::Or;

        if !self.skip_token(TokenKind::Symbol(symbol)) {
            return Some(lhs);
        }

        Some(Expr::Op(
            symbol,
            Box::new(lhs),
            Some(Box::new(self.parse_expr_bitwise_or()?)),
        ))
    }

    /// exprAssign   ::= exprBitwiseOr [ relationalOp exprRelational ]
    /// relationalOp ::= ">" | "<" | ">=" | "<=" | "==" | "!="
    fn parse_expr_relational(&mut self) -> Option<Expr> {
        use TokenSymbol::*;
        let lhs = self.parse_expr_bitwise_or()?;

        Some(match self.peek_token().kind {
            TokenKind::Symbol(symbol @ (Gt | Lt | GtEq | LtEq | EqEq | Ne)) => {
                self.next_token();
                Expr::Op(
                    symbol,
                    Box::new(lhs),
                    Some(Box::new(self.parse_expr_relational()?)),
                )
            }
            _ => lhs,
        })
    }

    /// exprLogicalAnd ::= exprLogicalRelational [ "&&" exprLogicalAnd ]
    fn parse_expr_logical_and(&mut self) -> Option<Expr> {
        let lhs = self.parse_expr_relational()?;
        let symbol = TokenSymbol::AndAnd;

        if !self.skip_token(TokenKind::Symbol(symbol)) {
            return Some(lhs);
        }

        Some(Expr::Op(
            symbol,
            Box::new(lhs),
            Some(Box::new(self.parse_expr_logical_and()?)),
        ))
    }

    /// exprLogicalOr ::= exprLogicalAnd [ "||" exprLogicalOr ]
    fn parse_expr_logical_or(&mut self) -> Option<Expr> {
        let lhs = self.parse_expr_logical_and()?;
        let symbol = TokenSymbol::OrOr;

        if !self.skip_token(TokenKind::Symbol(symbol)) {
            return Some(lhs);
        }

        Some(Expr::Op(
            symbol,
            Box::new(lhs),
            Some(Box::new(self.parse_expr_logical_or()?)),
        ))
    }

    /// exprAssign ::= exprLogicalOr [ assignOp exprAssign ]
    /// assignOp   ::= "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "^=" | "<<=" | ">>=" | "&=" | "|="
    fn parse_expr_assign(&mut self) -> Option<Expr> {
        use TokenSymbol::*;
        let lhs = self.parse_expr_logical_or()?;

        Some(match self.peek_token().kind {
            TokenKind::Symbol(
                symbol @ (Eq | PlusEq | MinusEq | StarEq | SlashEq | PercentEq | CaretEq | ShlEq
                | ShrEq | AndEq | OrEq),
            ) => {
                self.next_token();
                Expr::Op(
                    symbol,
                    Box::new(lhs),
                    Some(Box::new(self.parse_expr_assign()?)),
                )
            }
            _ => lhs,
        })
    }

    /// exprControl ::= "continue" | "break" | "return" [ exprControl ] | exprAssign
    fn parse_expr_control(&mut self) -> Option<Expr> {
        use TokenKeyword::*;

        Some(match self.peek_token().kind {
            TokenKind::Keyword(Continue) => {
                self.next_token();
                Expr::Continue
            }
            TokenKind::Keyword(Break) => {
                self.next_token();
                Expr::Break
            }
            TokenKind::Keyword(Return) => {
                self.next_token();
                Expr::Return(self.parse_expr_control().map(Box::new))
            }
            _ => self.parse_expr_assign()?,
        })
    }

    /// entrypoint for expression parsing using recursive descent parsing
    ///
    /// <https://en.wikipedia.org/wiki/Recursive_descent_parser>
    /// expr ::= exprControl
    pub(super) fn parse_expr(&mut self) -> Option<Expr> {
        self.parse_expr_control()
    }
}

#[test]
fn test_parse_expr() {
    use Literal::*;
    use TokenSymbol::*;

    macro_rules! b {
        ($expr:expr) => {
            Box::new($expr)
        };
    }

    let mut parser = Parser::new(
        r#"if if 1 { 1 } else { 0 } + 9 {
             a = 4
           } else if 1 {
             a = 5
           } else {
           }
          amul ^= (4 + 93 * (1 << 3) / 1.44) ^ bhatura
          stove = { 44 } + amul"#,
    );

    assert_eq!(
        parser.parse_expr(),
        Some(Expr::If(If {
            cond: b!(Expr::Op(
                Plus,
                b!(Expr::If(If {
                    cond: b!(Expr::Literal(Int(1))),
                    then: vec![Statement::Expr(Expr::Literal(Int(1)))],
                    or: Some(b!(ElseType::Else(vec![Statement::Expr(Expr::Literal(
                        Int(0)
                    ))])))
                })),
                Some(b!(Expr::Literal(Int(9))))
            )),
            then: vec![Statement::Expr(Expr::Op(
                Eq,
                b!(Expr::Identifier("a".into())),
                Some(b!(Expr::Literal(Int(4))))
            ))],
            or: Some(b!(ElseType::If(If {
                cond: b!(Expr::Literal(Int(1))),
                then: vec![Statement::Expr(Expr::Op(
                    Eq,
                    b!(Expr::Identifier("a".into())),
                    Some(b!(Expr::Literal(Int(5))))
                ))],
                or: Some(b!(ElseType::Else(vec![])))
            })))
        }))
    );

    assert_eq!(parser.skip_token(TokenKind::Newline), true);

    assert_eq!(
        parser.parse_expr(),
        Some(Expr::Op(
            CaretEq,
            b!(Expr::Identifier("amul".into())),
            Some(b!(Expr::Op(
                Caret,
                b!(Expr::Op(
                    Plus,
                    b!(Expr::Literal(Int(4))),
                    Some(b!(Expr::Op(
                        Star,
                        b!(Expr::Literal(Int(93))),
                        Some(b!(Expr::Op(
                            Slash,
                            b!(Expr::Op(
                                Shl,
                                b!(Expr::Literal(Int(1))),
                                Some(b!(Expr::Literal(Int(3))))
                            )),
                            Some(b!(Expr::Literal(Float(1.44))))
                        )))
                    )))
                )),
                Some(b!(Expr::Identifier("bhatura".into())))
            )))
        ))
    );

    assert_eq!(parser.skip_token(TokenKind::Newline), true);

    assert_eq!(
        parser.parse_expr(),
        Some(Expr::Op(
            Eq,
            b!(Expr::Identifier("stove".into())),
            Some(b!(Expr::Op(
                Plus,
                b!(Expr::Block(vec![Statement::Expr(Expr::Literal(Int(44)))])),
                Some(b!(Expr::Identifier("amul".into())))
            )))
        ))
    );
}

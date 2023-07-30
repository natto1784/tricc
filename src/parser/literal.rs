use super::Parser;
use crate::lexer::{
    TokenKind,
    TokenSymbol,
};

impl<'a> Parser<'a> {
    pub(super) fn parse_int(&mut self) -> Option<i32> {
        let val = self.next_token().val;
        let mut integer: i32 = 0;
        let error = || {
            self.error(&format!(
                "integer values must be in range [{}, {}]",
                i32::MIN,
                i32::MAX
            ))
        };

        for c in val.chars() {
            // c is always ['0'..='9']
            let d = c.to_digit(10)?;

            match integer.checked_mul(10) {
                Some(m) => integer = m,
                None => {
                    error();
                    return None;
                }
            }

            match integer.checked_add(d as i32) {
                Some(a) => integer = a,
                None => {
                    error();
                    return None;
                }
            }
        }
        Some(integer)
    }

    /// f32 can be NaN and inf as well
    pub(super) fn parse_float(&mut self) -> Option<f32> {
        let token = self.next_token();
        let mut chars = token.val.chars();
        let mut float: f32 = 0.0;
        let mut fraction: f32 = 0.0;
        let mut prec: i32 = 0;
        let mut exp: i32 = 0;
        let mut decimal: bool = false;

        // lexer takes care of multiple decimals and non digit characters
        for c in chars.by_ref() {
            match c {
                '.' => decimal = true,
                'e' | 'E' => {
                    // lexer takes care that decimal doesnt come after e
                    let s;
                    match self.peek_token().kind {
                        TokenKind::Symbol(TokenSymbol::Minus) => {
                            s = -1;
                            self.next_token();
                        }
                        TokenKind::Symbol(TokenSymbol::Plus) => {
                            s = 1;
                            self.next_token();
                        }
                        _ => s = 1,
                    }

                    exp = self.parse_int()? * s;
                    break;
                }
                _ => {
                    // c is always ['0'..='9']
                    let d = c.to_digit(10)? as f32;
                    if decimal {
                        fraction *= 10.0;
                        fraction += d;
                        prec += 1;
                    } else {
                        float *= 10.0;
                        float += d;
                    }
                }
            }
        }

        fraction /= 10f32.powi(prec);
        float += fraction;
        float *= 10f32.powi(exp);

        Some(float)
    }

    pub(super) fn parse_char(&mut self) -> Option<char> {
        // the lexer ensures that the 0th and 2nd characters are both '
        self.next_token().val.chars().nth(1)
    }
}

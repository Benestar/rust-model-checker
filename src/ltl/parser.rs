use super::lexer::*;
use super::Ltl;
use std::iter::Peekable;

/// Parse the given input string to an LTL formula.
pub fn parse(input: &str) -> ParserResult {
    Parser::new(Lexer::new(input.chars())).parse()
}

#[derive(Debug)]
pub enum ParserError {
    IllegalToken(Token),
    MissingParenthesis,
    UnexpectedEOF,
}

pub type ParserResult = Result<Ltl<String>, ParserError>;

struct Parser<I: Iterator> {
    tokens: Peekable<I>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new<T>(tokens: T) -> Self
    where
        T: IntoIterator<Item = Token, IntoIter = I>,
    {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> ParserResult {
        let expr = self.parse_prefix()?;

        // Look-ahead for binary operators
        if let Some(token) = self.tokens.peek() {
            if let LexItem::BinOp(op) = token.item {
                self.tokens.next().unwrap();

                return self.parse_binary_operator(op, expr);
            }
        }

        Ok(expr)
    }

    fn parse_prefix(&mut self) -> ParserResult {
        let token = self.expect_token()?;

        match token.item {
            LexItem::Paren(Parenthesis::Open) => self.parse_parenthesis(),
            LexItem::Lit(lit) => self.parse_literal(lit),
            LexItem::Var(var) => self.parse_variable(var),
            LexItem::UnOp(op) => self.parse_unary_operator(op),
            _ => Err(ParserError::IllegalToken(token)),
        }
    }

    fn parse_parenthesis(&mut self) -> ParserResult {
        let expr = self.parse()?;

        let token = self.expect_token()?;

        if token.item == LexItem::Paren(Parenthesis::Close) {
            Ok(expr)
        } else {
            Err(ParserError::MissingParenthesis)
        }
    }

    fn parse_literal(&mut self, lit: Literal) -> ParserResult {
        let expr = match lit {
            Literal::True => Ltl::True,
            Literal::False => Ltl::False,
        };

        Ok(expr)
    }

    fn parse_variable(&mut self, var: String) -> ParserResult {
        Ok(Ltl::Prop(var))
    }

    fn parse_unary_operator(&mut self, op: UnaryOperator) -> ParserResult {
        let inner = self.parse()?;

        let expr = match op {
            UnaryOperator::Not => Ltl::not(inner),
            UnaryOperator::Next => Ltl::next(inner),
            UnaryOperator::Finally => Ltl::finally(inner),
            UnaryOperator::Globally => Ltl::globally(inner),
        };

        Ok(expr)
    }

    fn parse_binary_operator(&mut self, op: BinaryOperator, left: Ltl<String>) -> ParserResult {
        let right = self.parse()?;

        let expr = match op {
            BinaryOperator::And => Ltl::and(left, right),
            BinaryOperator::Or => Ltl::or(left, right),
            BinaryOperator::Until => Ltl::until(left, right),
            BinaryOperator::Release => Ltl::release(left, right),
            BinaryOperator::WeakUntil => Ltl::weak_until(left, right),
            BinaryOperator::StrongRelease => Ltl::strong_release(left, right),
        };

        Ok(expr)
    }

    fn expect_token(&mut self) -> Result<Token, ParserError> {
        self.tokens.next().ok_or(ParserError::UnexpectedEOF)
    }
}

#[cfg(test)]
mod tests {
    use super::super::Ltl;
    use super::parse;

    #[test]
    fn test_parse() {
        assert_eq!(
            parse("a & b").unwrap(),
            Ltl::and(Ltl::Prop(String::from("a")), Ltl::Prop(String::from("b")))
        );

        assert_eq!(
            parse("F (a R b)").unwrap(),
            Ltl::finally(Ltl::release(
                Ltl::Prop(String::from("a")),
                Ltl::Prop(String::from("b"))
            ))
        );

        assert_eq!(
            parse("a & b W c & d").unwrap(),
            Ltl::and(
                Ltl::Prop(String::from("a")),
                Ltl::and(
                    Ltl::weak_until(Ltl::Prop(String::from("b")), Ltl::Prop(String::from("c"))),
                    Ltl::Prop(String::from("d"))
                )
            )
        );
    }
}

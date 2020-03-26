use super::lexer::*;
use super::Ltl;
use std::fmt;
use std::iter::Peekable;

/// Parse the given input string to an LTL formula.
pub fn parse(input: &str) -> ParserResult {
    Parser::new(Lexer::new(input.chars())).parse()
}

impl Operator {
    fn precedence(&self) -> usize {
        match self {
            Operator::Not => 80,
            Operator::And => 20,
            Operator::Or => 10,
            Operator::Next => 80,
            Operator::Finally => 80,
            Operator::Globally => 80,
            Operator::Until => 30,
            Operator::Release => 30,
            Operator::WeakUntil => 30,
            Operator::StrongRelease => 30,
        }
    }
}

/// The different errors that can occur during parsing
#[derive(Debug)]
pub enum ParserError {
    IllegalToken(Token),
    MissingParenthesis(Token, Token),
    NoUnaryOperator(Operator, Token),
    NoBinaryOperator(Operator, Token),
    TrailingGarbage(Token),
    UnexpectedEOF,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::IllegalToken(token) => write!(f, "Illegal token {}", token),
            ParserError::MissingParenthesis(open, close) => write!(
                f,
                "Missing closing parenthesis (found {}, opened at {})",
                close, open.pos
            ),
            ParserError::NoUnaryOperator(_, token) => write!(f, "Not an unary operator: {}", token),
            ParserError::NoBinaryOperator(_, token) => {
                write!(f, "Not a binary operator: {}", token)
            }
            ParserError::TrailingGarbage(token) => {
                write!(f, "Trailing garbage at end of file: {}", token)
            }
            ParserError::UnexpectedEOF => write!(f, "Unexpected end of file"),
        }
    }
}

/// The result of the LTL parser
pub type ParserResult = Result<Ltl<String>, ParserError>;

/// Pratt parser for LTL formulas
struct Parser<I: Iterator> {
    tokens: Peekable<I>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    /// Create a new parser for the given token stream.
    pub fn new<T>(tokens: T) -> Self
    where
        T: IntoIterator<Item = Token, IntoIter = I>,
    {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    /// Parse the LTL formula.
    pub fn parse(&mut self) -> ParserResult {
        let res = self.parse_all(0);

        if let Some(token) = self.tokens.next() {
            return Err(ParserError::TrailingGarbage(token));
        }

        res
    }

    fn parse_all(&mut self, precedence: usize) -> ParserResult {
        let mut expr = self.parse_prefix()?;

        // Look-ahead for binary operators
        while precedence < self.get_precedence() {
            let token = self.tokens.next().unwrap();

            if let TokenType::Op(op) = token.typ {
                expr = self.parse_binary_operator(op, token, expr)?;
            }
        }

        Ok(expr)
    }

    fn get_precedence(&mut self) -> usize {
        if let Some(token) = self.tokens.peek() {
            if let TokenType::Op(op) = token.typ {
                return op.precedence();
            }
        }

        0
    }

    fn parse_prefix(&mut self) -> ParserResult {
        let token = self.expect_token()?;

        match token.typ {
            TokenType::Par(Parenthesis::Open) => self.parse_parenthesis(token),
            TokenType::Lit(lit) => self.parse_literal(lit),
            TokenType::Var => self.parse_variable(token),
            TokenType::Op(op) => self.parse_unary_operator(op, token),
            _ => Err(ParserError::IllegalToken(token)),
        }
    }

    fn parse_parenthesis(&mut self, open: Token) -> ParserResult {
        let expr = self.parse_all(0)?;

        let close = self.expect_token()?;

        if close.typ == TokenType::Par(Parenthesis::Close) {
            Ok(expr)
        } else {
            Err(ParserError::MissingParenthesis(open, close))
        }
    }

    fn parse_literal(&mut self, lit: Literal) -> ParserResult {
        let expr = match lit {
            Literal::True => Ltl::True,
            Literal::False => Ltl::False,
        };

        Ok(expr)
    }

    fn parse_variable(&mut self, token: Token) -> ParserResult {
        Ok(Ltl::Prop(token.val))
    }

    fn parse_unary_operator(&mut self, op: Operator, token: Token) -> ParserResult {
        let inner = self.parse_all(op.precedence())?;

        let expr = match op {
            Operator::Not => Ltl::not(inner),
            Operator::Next => Ltl::next(inner),
            Operator::Finally => Ltl::finally(inner),
            Operator::Globally => Ltl::globally(inner),
            _ => return Err(ParserError::NoUnaryOperator(op, token)),
        };

        Ok(expr)
    }

    fn parse_binary_operator(
        &mut self,
        op: Operator,
        token: Token,
        left: Ltl<String>,
    ) -> ParserResult {
        let right = self.parse_all(op.precedence() - 1)?;

        let expr = match op {
            Operator::And => Ltl::and(left, right),
            Operator::Or => Ltl::or(left, right),
            Operator::Until => Ltl::until(left, right),
            Operator::Release => Ltl::release(left, right),
            Operator::WeakUntil => Ltl::weak_until(left, right),
            Operator::StrongRelease => Ltl::strong_release(left, right),
            _ => return Err(ParserError::NoBinaryOperator(op, token)),
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

use super::lexer::*;
use super::Ltl;
use std::fmt;
use std::iter::Peekable;

/// Parse the given input string to an LTL formula.
pub fn parse(input: &str) -> ParserResult {
    Parser::new(Lexer::new(input.chars())).parse()
}

/// Define the precedence of operators
trait Precedence {
    fn precedence(&self) -> usize;
}

impl Precedence for UnaryOperator {
    fn precedence(&self) -> usize {
        80
    }
}

impl Precedence for BinaryOperator {
    fn precedence(&self) -> usize {
        match self {
            BinaryOperator::And => 20,
            BinaryOperator::Or => 10,
            BinaryOperator::Until => 30,
            BinaryOperator::Release => 30,
            BinaryOperator::WeakUntil => 30,
            BinaryOperator::StrongRelease => 30,
        }
    }
}

/// The different errors that can occur during parsing
#[derive(Debug)]
pub enum ParserError {
    IllegalToken(Token),
    MissingParenthesis(Token, Token),
    UnexpectedEOF,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::IllegalToken(token) => write!(
                f,
                "Illegal token found at {} (found {:?})",
                token.position, token.item
            ),
            ParserError::MissingParenthesis(open, close) => write!(
                f,
                "Missing closing parenthesis for {} at {} (found {:?})",
                open.position, close.position, close.item
            ),
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
        self.parse_all(0)
    }

    fn parse_all(&mut self, precedence: usize) -> ParserResult {
        let mut expr = self.parse_prefix()?;

        // Look-ahead for binary operators
        while precedence < self.get_precedence() {
            if let LexItem::BinOp(op) = self.tokens.next().unwrap().item {
                expr = self.parse_binary_operator(op, expr)?;
            }
        }

        Ok(expr)
    }

    fn get_precedence(&mut self) -> usize {
        if let Some(token) = self.tokens.peek() {
            if let LexItem::BinOp(op) = token.item {
                return op.precedence();
            }
        }

        0
    }

    fn parse_prefix(&mut self) -> ParserResult {
        let token = self.expect_token()?;

        match token.item {
            LexItem::Paren(Parenthesis::Open) => self.parse_parenthesis(token),
            LexItem::Lit(lit) => self.parse_literal(lit),
            LexItem::Var(var) => self.parse_variable(var),
            LexItem::UnOp(op) => self.parse_unary_operator(op),
            _ => Err(ParserError::IllegalToken(token)),
        }
    }

    fn parse_parenthesis(&mut self, open: Token) -> ParserResult {
        let expr = self.parse()?;

        let close = self.expect_token()?;

        if close.item == LexItem::Paren(Parenthesis::Close) {
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

    fn parse_variable(&mut self, var: String) -> ParserResult {
        Ok(Ltl::Prop(var))
    }

    fn parse_unary_operator(&mut self, op: UnaryOperator) -> ParserResult {
        let inner = self.parse_all(op.precedence())?;

        let expr = match op {
            UnaryOperator::Not => Ltl::not(inner),
            UnaryOperator::Next => Ltl::next(inner),
            UnaryOperator::Finally => Ltl::finally(inner),
            UnaryOperator::Globally => Ltl::globally(inner),
        };

        Ok(expr)
    }

    fn parse_binary_operator(&mut self, op: BinaryOperator, left: Ltl<String>) -> ParserResult {
        let right = self.parse_all(op.precedence() - 1)?;

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

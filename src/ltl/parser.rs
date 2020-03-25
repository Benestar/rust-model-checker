use super::lexer::*;
use super::Ltl;
use std::iter::Peekable;

#[derive(Debug)]
pub struct ParserError;

// Plan (recursive descent parser):
//
// - We first parse a full expression (recursively)
// - Afterwards, we check whether it is followed by a binary operator
// - If that is the case, we parse the binary operator and another full expression.
// - After that we must return (and fail at the top-level if there are still tokens coming)

pub fn parse(
    stream: &mut Peekable<impl Iterator<Item = (usize, LexItem)>>,
) -> Result<Ltl<String>, ParserError> {
    let (i, token) = stream.next().ok_or(ParserError)?;

    let expr = match token {
        LexItem::Paren(Parenthesis::Open) => {
            // parse expression
            let expr = parse(stream)?;

            // check for closing parenthesis
            let (i, token) = stream.next().ok_or(ParserError)?;

            if token != LexItem::Paren(Parenthesis::Close) {
                return Err(ParserError);
            }

            expr
        }
        LexItem::UnOp(op) => {
            // parse expression
            let expr = parse(stream)?;

            // wrap in unary operator
            match op {
                UnaryOperator::Not => Ltl::not(expr),
                UnaryOperator::Next => Ltl::next(expr),
                UnaryOperator::Finally => Ltl::finally(expr),
                UnaryOperator::Globally => Ltl::globally(expr),
            }
        }
        LexItem::Lit(literal) => {
            // return literal
            match literal {
                Literal::True => Ltl::True,
                Literal::False => Ltl::False,
            }
        }
        LexItem::Var(var) => Ltl::Prop(var),
        _ => return Err(ParserError),
    };

    // parse binary operator (and second expression)
    if let Some(&(i, LexItem::BinOp(op))) = stream.peek() {
        stream.next();

        // parse second expression
        let expr2 = parse(stream)?;

        // wrap in binary operator
        let expr = match op {
            BinaryOperator::And => Ltl::and(expr, expr2),
            BinaryOperator::Or => Ltl::or(expr, expr2),
            BinaryOperator::Until => Ltl::until(expr, expr2),
            BinaryOperator::Release => Ltl::release(expr, expr2),
            _ => return Err(ParserError),
        };

        Ok(expr)
    } else {
        Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use super::super::lexer::*;
    use super::super::Ltl;
    use super::parse;

    #[test]
    fn parse_ltl() {
        assert_eq!(
            parse(
                &mut vec![
                    (0, LexItem::Var(String::from("a"))),
                    (1, LexItem::BinOp(BinaryOperator::And)),
                    (2, LexItem::Var(String::from("b"))),
                ]
                .into_iter()
                .peekable()
            )
            .unwrap(),
            Ltl::and(Ltl::Prop(String::from("a")), Ltl::Prop(String::from("b")))
        );
    }
}

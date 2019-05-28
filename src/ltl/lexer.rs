//! Lexer for LTL formulas
//!
//! The function [`lex`] turns an input string into a list of LTL tokens as
//! described in the [`LexItem`] enum.
//! On Error, a [`LexerError`] is returned with information about the illegal
//! character.
//!
//! [`lex`]: fn.lex.html
//! [`LexItem`]: enum.LexItem.html
//! [`LexerError`]: struct.LexerError.html

use std::iter::{Enumerate, Peekable};
use std::str::Chars;

/// Enumeration of different LTL tokens
///
/// The following tokens are supported:
///
/// | Token type       | Allowed values
/// | ---------------- | --------------
/// | Parentheses      | `(`, `)`
/// | Unary operators  | `~`, `X`, `F`, `G`
/// | Binary Operators | `&`, <code>\|</code>, `U`, `R`
/// | Literals         | `true`, `false`, `1`, `0`
/// | Variables        | `abc`, ...
#[derive(Debug, PartialEq)]
pub enum LexItem {
    Paren(Parenthesis),
    UnOp(UnaryOperator),
    BinOp(BinaryOperator),
    Lit(Literal),
    Var(String),
}

#[derive(Debug, PartialEq)]
pub enum Parenthesis {
    Open,
    Close,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Not,
    Next,
    Finally,
    Globally,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    And,
    Or,
    Until,
    Release,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    True,
    False,
}

impl Parenthesis {
    fn new(s: &str) -> Option<Self> {
        match s {
            "(" => Some(Parenthesis::Open),
            ")" => Some(Parenthesis::Close),
            _ => None,
        }
    }
}

impl UnaryOperator {
    fn new(s: &str) -> Option<Self> {
        match s {
            "!" | "¬" | "~" => Some(UnaryOperator::Not),
            "X" => Some(UnaryOperator::Next),
            "F" | "<>" => Some(UnaryOperator::Finally),
            "G" | "[]" => Some(UnaryOperator::Globally),
            _ => None,
        }
    }
}

impl BinaryOperator {
    fn new(s: &str) -> Option<Self> {
        match s {
            "&" | "∧" | "/\\" => Some(BinaryOperator::And),
            "|" | "∨" | "\\/" => Some(BinaryOperator::Or),
            "U" => Some(BinaryOperator::Until),
            "R" => Some(BinaryOperator::Release),
            _ => None,
        }
    }
}

impl Literal {
    fn new(s: &str) -> Option<Self> {
        match s {
            "1" | "true" => Some(Literal::True),
            "0" | "false" => Some(Literal::False),
            _ => None,
        }
    }
}

/// Error on illegal character
///
/// This error contains the position in the input string and the illegal
/// character.
#[derive(Debug, PartialEq)]
pub struct LexerError {
    pos: usize,
    chr: char,
}

/// Turn an input string into a list of tokens and their position.
pub fn lex(input: &str) -> Result<Vec<(usize, LexItem)>, LexerError> {
    let mut result = Vec::new();

    let mut it = input.chars().enumerate().peekable();

    while let Some(&(i, c)) = it.peek() {
        if c.is_whitespace() {
            it.next();
            continue;
        }

        let token = match c {
            'a'...'z' => {
                let word = lex_lowercase_alphanumeric(&mut it);

                if let Some(lit) = Literal::new(&word) {
                    LexItem::Lit(lit)
                } else {
                    LexItem::Var(word)
                }
            }
            '1' | '0' => {
                it.next();

                LexItem::Lit(Literal::new(&c.to_string()).unwrap())
            }
            '~' | 'X' | 'F' | 'G' => {
                it.next();

                LexItem::UnOp(UnaryOperator::new(&c.to_string()).unwrap())
            }
            '&' | '|' | 'U' | 'R' => {
                it.next();

                LexItem::BinOp(BinaryOperator::new(&c.to_string()).unwrap())
            }
            '(' | ')' => {
                it.next();

                LexItem::Paren(Parenthesis::new(&c.to_string()).unwrap())
            }
            _ => return Err(LexerError { pos: i, chr: c }),
        };

        result.push((i, token));
    }

    Ok(result)
}

fn lex_lowercase_alphanumeric(it: &mut Peekable<Enumerate<Chars>>) -> String {
    let mut token = String::new();

    while let Some(&(_, c)) = it.peek() {
        match c {
            'a'...'z' | '0'...'9' | '_' => token.push(c),
            _ => break,
        }

        it.next();
    }

    token
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_ltl() {
        assert_eq!(lex("").unwrap(), vec![]);

        assert_eq!(lex("  \n  \t\r").unwrap(), vec![]);

        assert_eq!(
            lex("a_bc123 ").unwrap(),
            vec![(0, LexItem::Var(String::from("a_bc123")))]
        );

        assert_eq!(
            lex("abc a\nb \t   c").unwrap(),
            vec![
                (0, LexItem::Var(String::from("abc"))),
                (4, LexItem::Var(String::from("a"))),
                (6, LexItem::Var(String::from("b"))),
                (12, LexItem::Var(String::from("c"))),
            ]
        );

        assert_eq!(
            lex("true false trueee").unwrap(),
            vec![
                (0, LexItem::Lit(Literal::True)),
                (5, LexItem::Lit(Literal::False)),
                (11, LexItem::Var(String::from("trueee"))),
            ]
        );

        assert_eq!(
            lex("11010").unwrap(),
            vec![
                (0, LexItem::Lit(Literal::True)),
                (1, LexItem::Lit(Literal::True)),
                (2, LexItem::Lit(Literal::False)),
                (3, LexItem::Lit(Literal::True)),
                (4, LexItem::Lit(Literal::False)),
            ]
        );

        assert_eq!(
            lex("U").unwrap(),
            vec![(0, LexItem::BinOp(BinaryOperator::Until))]
        );

        assert_eq!(
            lex("())()").unwrap(),
            vec![
                (0, LexItem::Paren(Parenthesis::Open)),
                (1, LexItem::Paren(Parenthesis::Close)),
                (2, LexItem::Paren(Parenthesis::Close)),
                (3, LexItem::Paren(Parenthesis::Open)),
                (4, LexItem::Paren(Parenthesis::Close)),
            ]
        );

        assert_eq!(
            lex("a U (b & c)").unwrap(),
            vec![
                (0, LexItem::Var(String::from("a"))),
                (2, LexItem::BinOp(BinaryOperator::Until)),
                (4, LexItem::Paren(Parenthesis::Open)),
                (5, LexItem::Var(String::from("b"))),
                (7, LexItem::BinOp(BinaryOperator::And)),
                (9, LexItem::Var(String::from("c"))),
                (10, LexItem::Paren(Parenthesis::Close)),
            ]
        );

        assert_eq!(
            lex("xR~y").unwrap(),
            vec![
                (0, LexItem::Var(String::from("x"))),
                (1, LexItem::BinOp(BinaryOperator::Release)),
                (2, LexItem::UnOp(UnaryOperator::Not)),
                (3, LexItem::Var(String::from("y"))),
            ]
        );
    }

    #[test]
    fn lex_ltl_error() {
        assert_eq!(lex("A").unwrap_err(), LexerError { pos: 0, chr: 'A' });

        assert_eq!(lex("4abc").unwrap_err(), LexerError { pos: 0, chr: '4' });

        assert_eq!(lex("(((24").unwrap_err(), LexerError { pos: 3, chr: '2' });
    }
}

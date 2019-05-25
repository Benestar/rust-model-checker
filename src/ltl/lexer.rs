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
/// | Token type  | Allowed values
/// | ----------- | --------------
/// | Parentheses | `(`, `)`
/// | Operators   | `&`, <code>\|</code>, `~`, `X`, `F`, `G`, `U`, `R`
/// | Literals    | `true`, `false`, `1`, `0`
/// | Variables   | `abc`, ...
#[derive(Clone, Debug, PartialEq)]
pub enum LexItem {
    Paren(char),
    Op(char),
    Lit(String),
    Var(String),
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

const LITERALS: [&str; 2] = ["true", "false"];

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
                let word = lex_word(&mut it);

                if LITERALS.contains(&word.as_str()) {
                    LexItem::Lit(word)
                } else {
                    LexItem::Var(word)
                }
            }
            '1' | '0' => {
                it.next();

                LexItem::Lit(c.to_string())
            }
            '&' | '|' | '~' | 'X' | 'F' | 'G' | 'U' | 'R' => {
                it.next();

                LexItem::Op(c)
            }
            '(' | ')' => {
                it.next();

                LexItem::Paren(c)
            }
            _ => return Err(LexerError { pos: i, chr: c }),
        };

        result.push((i, token));
    }

    Ok(result)
}

fn lex_word(it: &mut Peekable<Enumerate<Chars>>) -> String {
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
    use super::LexItem::*;
    use super::{lex, LexerError};

    #[test]
    fn lex_ltl() {
        assert_eq!(lex("").unwrap(), vec![]);

        assert_eq!(lex("  \n  \t\r").unwrap(), vec![]);

        assert_eq!(
            lex("a_bc123 ").unwrap(),
            vec![(0, Var("a_bc123".to_string()))]
        );

        assert_eq!(
            lex("abc a\nb \t   c").unwrap(),
            vec![
                (0, Var("abc".to_string())),
                (4, Var("a".to_string())),
                (6, Var("b".to_string())),
                (12, Var("c".to_string())),
            ]
        );

        assert_eq!(
            lex("true false trueee").unwrap(),
            vec![
                (0, Lit("true".to_string())),
                (5, Lit("false".to_string())),
                (11, Var("trueee".to_string())),
            ]
        );

        assert_eq!(
            lex("11010").unwrap(),
            vec![
                (0, Lit("1".to_string())),
                (1, Lit("1".to_string())),
                (2, Lit("0".to_string())),
                (3, Lit("1".to_string())),
                (4, Lit("0".to_string())),
            ]
        );

        assert_eq!(lex("U").unwrap(), vec![(0, Op('U'))]);

        assert_eq!(
            lex("())()").unwrap(),
            vec![
                (0, Paren('(')),
                (1, Paren(')')),
                (2, Paren(')')),
                (3, Paren('(')),
                (4, Paren(')')),
            ]
        );

        assert_eq!(
            lex("a U (b & c)").unwrap(),
            vec![
                (0, Var("a".to_string())),
                (2, Op('U')),
                (4, Paren('(')),
                (5, Var("b".to_string())),
                (7, Op('&')),
                (9, Var("c".to_string())),
                (10, Paren(')')),
            ]
        );

        assert_eq!(
            lex("xRy").unwrap(),
            vec![
                (0, Var("x".to_string())),
                (1, Op('R')),
                (2, Var("y".to_string())),
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

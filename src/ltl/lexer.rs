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

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub item: LexItem,
    pub position: usize,
}

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
#[derive(Clone, Debug, PartialEq)]
pub enum LexItem {
    Paren(Parenthesis),
    UnOp(UnaryOperator),
    BinOp(BinaryOperator),
    Lit(Literal),
    Var(String),
    Unk(String),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Parenthesis {
    Open,
    Close,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOperator {
    Not,
    Next,
    Finally,
    Globally,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOperator {
    And,
    Or,
    Until,
    Release,
    WeakUntil,
    StrongRelease,
}

#[derive(Clone, Copy, Debug, PartialEq)]
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
            "W" => Some(BinaryOperator::WeakUntil),
            "M" => Some(BinaryOperator::StrongRelease),
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

/// A Lexer over an iterator of chars
pub struct Lexer<I: Iterator> {
    iter: Peekable<Enumerate<I>>,
}

impl<I> Lexer<I>
where
    I: Iterator<Item = char>,
{
    /// Create a new Lexer from the given iterator.
    pub fn new<T>(input: T) -> Self
    where
        T: IntoIterator<Item = char, IntoIter = I>,
    {
        Self {
            iter: input.into_iter().enumerate().peekable(),
        }
    }

    fn read_lex_item(&mut self, c: char) -> LexItem {
        match c {
            'a'..='z' => {
                let word = self.read_alphanumeric();

                if let Some(lit) = Literal::new(&word) {
                    LexItem::Lit(lit)
                } else {
                    LexItem::Var(word)
                }
            }
            '1' | '0' => {
                self.iter.next();

                LexItem::Lit(Literal::new(&c.to_string()).unwrap())
            }
            '~' | 'X' | 'F' | 'G' => {
                self.iter.next();

                LexItem::UnOp(UnaryOperator::new(&c.to_string()).unwrap())
            }
            '&' | '|' | 'U' | 'R' | 'W' | 'M' => {
                self.iter.next();

                LexItem::BinOp(BinaryOperator::new(&c.to_string()).unwrap())
            }
            '(' | ')' => {
                self.iter.next();

                LexItem::Paren(Parenthesis::new(&c.to_string()).unwrap())
            }
            _ => {
                let unk = self.read_no_whitespace();

                LexItem::Unk(unk)
            }
        }
    }

    fn read_alphanumeric(&mut self) -> String {
        let mut token = String::new();

        while let Some(&(_, c)) = self.iter.peek() {
            match c {
                'a'..='z' | '0'..='9' | '_' => token.push(c),
                _ => break,
            }

            self.iter.next();
        }

        token
    }

    fn read_no_whitespace(&mut self) -> String {
        let mut token = String::new();

        while let Some(&(_, c)) = self.iter.peek() {
            if c.is_whitespace() {
                break;
            }

            token.push(c);

            self.iter.next();
        }

        token
    }

    fn skip_whitespace(&mut self) {
        while self
            .iter
            .peek()
            .filter(|&(_, c)| c.is_whitespace())
            .is_some()
        {
            self.iter.next();
        }
    }
}

impl<I> Iterator for Lexer<I>
where
    I: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        self.iter.peek().copied().map(|(i, c)| Token {
            item: self.read_lex_item(c),
            position: i,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let vec: Vec<_> = Lexer::new("".chars()).collect();

        assert_eq!(vec, vec![]);

        let vec: Vec<_> = Lexer::new("  \n  \t\r".chars()).collect();

        assert_eq!(vec, vec![]);

        let vec: Vec<_> = Lexer::new("a_bc123 ".chars()).collect();

        assert_eq!(
            vec,
            vec![Token {
                item: LexItem::Var(String::from("a_bc123")),
                position: 0,
            }]
        );

        let vec: Vec<_> = Lexer::new("abc a\nb \t   c".chars()).collect();

        assert_eq!(
            vec,
            vec![
                Token {
                    item: LexItem::Var(String::from("abc")),
                    position: 0,
                },
                Token {
                    item: LexItem::Var(String::from("a")),
                    position: 4,
                },
                Token {
                    item: LexItem::Var(String::from("b")),
                    position: 6,
                },
                Token {
                    item: LexItem::Var(String::from("c")),
                    position: 12,
                },
            ]
        );

        let vec: Vec<_> = Lexer::new("true false trueee".chars()).collect();

        assert_eq!(
            vec,
            vec![
                Token {
                    item: LexItem::Lit(Literal::True),
                    position: 0,
                },
                Token {
                    item: LexItem::Lit(Literal::False),
                    position: 5,
                },
                Token {
                    item: LexItem::Var(String::from("trueee")),
                    position: 11,
                },
            ]
        );

        let vec: Vec<_> = Lexer::new("11010".chars()).collect();

        assert_eq!(
            vec,
            vec![
                Token {
                    item: LexItem::Lit(Literal::True),
                    position: 0,
                },
                Token {
                    item: LexItem::Lit(Literal::True),
                    position: 1,
                },
                Token {
                    item: LexItem::Lit(Literal::False),
                    position: 2,
                },
                Token {
                    item: LexItem::Lit(Literal::True),
                    position: 3,
                },
                Token {
                    item: LexItem::Lit(Literal::False),
                    position: 4,
                },
            ]
        );

        let vec: Vec<_> = Lexer::new("U".chars()).collect();

        assert_eq!(
            vec,
            vec![Token {
                item: LexItem::BinOp(BinaryOperator::Until),
                position: 0,
            }]
        );

        let vec: Vec<_> = Lexer::new("())()".chars()).collect();

        assert_eq!(
            vec,
            vec![
                Token {
                    item: LexItem::Paren(Parenthesis::Open),
                    position: 0,
                },
                Token {
                    item: LexItem::Paren(Parenthesis::Close),
                    position: 1,
                },
                Token {
                    item: LexItem::Paren(Parenthesis::Close),
                    position: 2,
                },
                Token {
                    item: LexItem::Paren(Parenthesis::Open),
                    position: 3,
                },
                Token {
                    item: LexItem::Paren(Parenthesis::Close),
                    position: 4,
                },
            ]
        );

        let vec: Vec<_> = Lexer::new("a U (b & c)".chars()).collect();

        assert_eq!(
            vec,
            vec![
                Token {
                    item: LexItem::Var(String::from("a")),
                    position: 0,
                },
                Token {
                    item: LexItem::BinOp(BinaryOperator::Until),
                    position: 2,
                },
                Token {
                    item: LexItem::Paren(Parenthesis::Open),
                    position: 4,
                },
                Token {
                    item: LexItem::Var(String::from("b")),
                    position: 5,
                },
                Token {
                    item: LexItem::BinOp(BinaryOperator::And),
                    position: 7,
                },
                Token {
                    item: LexItem::Var(String::from("c")),
                    position: 9,
                },
                Token {
                    item: LexItem::Paren(Parenthesis::Close),
                    position: 10,
                },
            ]
        );

        let vec: Vec<_> = Lexer::new("xR~y".chars()).collect();

        assert_eq!(
            vec,
            vec![
                Token {
                    item: LexItem::Var(String::from("x")),
                    position: 0,
                },
                Token {
                    item: LexItem::BinOp(BinaryOperator::Release),
                    position: 1,
                },
                Token {
                    item: LexItem::UnOp(UnaryOperator::Not),
                    position: 2,
                },
                Token {
                    item: LexItem::Var(String::from("y")),
                    position: 3,
                },
            ]
        );

        let vec: Vec<_> = Lexer::new("A".chars()).collect();

        assert_eq!(
            vec,
            vec![Token {
                item: LexItem::Unk("A".to_string()),
                position: 0,
            }]
        );

        let vec: Vec<_> = Lexer::new("4abc".chars()).collect();

        assert_eq!(
            vec,
            vec![Token {
                item: LexItem::Unk("4abc".to_string()),
                position: 0,
            }]
        );

        let vec: Vec<_> = Lexer::new("(((24".chars()).collect();

        assert_eq!(
            vec,
            vec![
                Token {
                    item: LexItem::Paren(Parenthesis::Open),
                    position: 0,
                },
                Token {
                    item: LexItem::Paren(Parenthesis::Open),
                    position: 1,
                },
                Token {
                    item: LexItem::Paren(Parenthesis::Open),
                    position: 2,
                },
                Token {
                    item: LexItem::Unk("24".to_string()),
                    position: 3,
                }
            ]
        );
    }
}

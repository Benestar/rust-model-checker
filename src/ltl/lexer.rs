//! Lexer for LTL formulas
//!
//! The [`Lexer`] struct takes an iterator over a sequence of characters and turns it into an iterator of LTL tokens,
//! as defined in the [`Token`] struct. A token has a position, a string representation and a token type.
//! The enum [`TokenType`] describes the various available types of tokens supported by the lexer.
//!
//! [`Lexer`]: struct.Lexer.html
//! [`Token`]: struct.Token.html
//! [`TokenType`]: enum.TokenType.html

use std::fmt;
use std::iter::{Enumerate, Peekable};

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub pos: usize,
    pub val: String,
    pub typ: TokenType,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Par(Parenthesis),
    Op(Operator),
    Lit(Literal),
    Var,
    Unk,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Parenthesis {
    Open,
    Close,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    Not,
    And,
    Or,
    Next,
    Finally,
    Globally,
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

impl Operator {
    fn new(s: &str) -> Option<Self> {
        match s {
            "!" | "¬" | "~" => Some(Operator::Not),
            "&" | "∧" | "/\\" => Some(Operator::And),
            "|" | "∨" | "\\/" => Some(Operator::Or),
            "X" | "()" => Some(Operator::Next),
            "F" | "<>" => Some(Operator::Finally),
            "G" | "[]" => Some(Operator::Globally),
            "U" => Some(Operator::Until),
            "R" => Some(Operator::Release),
            "W" => Some(Operator::WeakUntil),
            "M" => Some(Operator::StrongRelease),
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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\" ({}) at pos {}", self.val, self.typ, self.pos)
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::Par(par) => par.fmt(f),
            TokenType::Op(op) => op.fmt(f),
            TokenType::Lit(lit) => lit.fmt(f),
            TokenType::Var => write!(f, "variable"),
            TokenType::Unk => write!(f, "unknown"),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::True => write!(f, "true literal"),
            Literal::False => write!(f, "false literal"),
        }
    }
}

impl fmt::Display for Parenthesis {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Parenthesis::Open => write!(f, "opening parenthesis"),
            Parenthesis::Close => write!(f, "closing parenthesis"),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operator::Not => write!(f, "not operator"),
            Operator::And => write!(f, "and operator"),
            Operator::Or => write!(f, "or operator"),
            Operator::Next => write!(f, "next operator"),
            Operator::Finally => write!(f, "finally operator"),
            Operator::Globally => write!(f, "globally operator"),
            Operator::Until => write!(f, "until operator"),
            Operator::Release => write!(f, "release operator"),
            Operator::WeakUntil => write!(f, "weak until operator"),
            Operator::StrongRelease => write!(f, "strong release operator"),
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

    /// Read the next token and return its type and string representation.
    fn read_token(&mut self, c: char) -> (TokenType, String) {
        match c {
            'a'..='z' => {
                let val = self.read_alphanumeric();

                let typ = if let Some(lit) = Literal::new(&val) {
                    TokenType::Lit(lit)
                } else {
                    TokenType::Var
                };

                (typ, val)
            }
            '1' | '0' => {
                self.iter.next();

                let val = c.to_string();
                let typ = TokenType::Lit(Literal::new(&val).unwrap());

                (typ, val)
            }
            '~' | 'X' | 'F' | 'G' => {
                self.iter.next();

                let val = c.to_string();
                let typ = TokenType::Op(Operator::new(&val).unwrap());

                (typ, val)
            }
            '&' | '|' | 'U' | 'R' | 'W' | 'M' => {
                self.iter.next();

                let val = c.to_string();
                let typ = TokenType::Op(Operator::new(&val).unwrap());

                (typ, val)
            }
            '(' | ')' => {
                self.iter.next();

                let val = c.to_string();
                let typ = TokenType::Par(Parenthesis::new(&val).unwrap());

                (typ, val)
            }
            _ => {
                let val = self.read_until_whitespace();
                let typ = TokenType::Unk;

                (typ, val)
            }
        }
    }

    /// Read an alphanumeric value from the char stream.
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

    /// Read characters until a whitespace character appears on the char stream.
    fn read_until_whitespace(&mut self) -> String {
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

    /// Skip characters until a whitespace character appears on the char stream.
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

        self.iter.peek().copied().map(|(pos, chr)| {
            let (typ, val) = self.read_token(chr);

            Token { typ, val, pos }
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
                typ: TokenType::Var,
                pos: 0,
                val: String::from("a_bc123"),
            }]
        );

        let vec: Vec<_> = Lexer::new("abc a\nb \t   c".chars()).collect();

        assert_eq!(
            vec,
            vec![
                Token {
                    typ: TokenType::Var,
                    pos: 0,
                    val: String::from("abc"),
                },
                Token {
                    typ: TokenType::Var,
                    pos: 4,
                    val: String::from("a"),
                },
                Token {
                    typ: TokenType::Var,
                    pos: 6,
                    val: String::from("b"),
                },
                Token {
                    typ: TokenType::Var,
                    pos: 12,
                    val: String::from("c"),
                },
            ]
        );

        let vec: Vec<_> = Lexer::new("true false trueee".chars()).collect();

        assert_eq!(
            vec,
            vec![
                Token {
                    typ: TokenType::Lit(Literal::True),
                    pos: 0,
                    val: String::from("true"),
                },
                Token {
                    typ: TokenType::Lit(Literal::False),
                    pos: 5,
                    val: String::from("false"),
                },
                Token {
                    typ: TokenType::Var,
                    pos: 11,
                    val: String::from("trueee"),
                },
            ]
        );

        let vec: Vec<_> = Lexer::new("11010".chars()).collect();

        assert_eq!(
            vec,
            vec![
                Token {
                    typ: TokenType::Lit(Literal::True),
                    pos: 0,
                    val: String::from("1"),
                },
                Token {
                    typ: TokenType::Lit(Literal::True),
                    pos: 1,
                    val: String::from("1"),
                },
                Token {
                    typ: TokenType::Lit(Literal::False),
                    pos: 2,
                    val: String::from("0"),
                },
                Token {
                    typ: TokenType::Lit(Literal::True),
                    pos: 3,
                    val: String::from("1"),
                },
                Token {
                    typ: TokenType::Lit(Literal::False),
                    pos: 4,
                    val: String::from("0"),
                },
            ]
        );

        let vec: Vec<_> = Lexer::new("U".chars()).collect();

        assert_eq!(
            vec,
            vec![Token {
                typ: TokenType::Op(Operator::Until),
                pos: 0,
                val: String::from("U"),
            }]
        );

        let vec: Vec<_> = Lexer::new("())()".chars()).collect();

        assert_eq!(
            vec,
            vec![
                Token {
                    typ: TokenType::Par(Parenthesis::Open),
                    pos: 0,
                    val: String::from("("),
                },
                Token {
                    typ: TokenType::Par(Parenthesis::Close),
                    pos: 1,
                    val: String::from(")"),
                },
                Token {
                    typ: TokenType::Par(Parenthesis::Close),
                    pos: 2,
                    val: String::from(")"),
                },
                Token {
                    typ: TokenType::Par(Parenthesis::Open),
                    pos: 3,
                    val: String::from("("),
                },
                Token {
                    typ: TokenType::Par(Parenthesis::Close),
                    pos: 4,
                    val: String::from(")"),
                },
            ]
        );

        let vec: Vec<_> = Lexer::new("a U (b & c)".chars()).collect();

        assert_eq!(
            vec,
            vec![
                Token {
                    typ: TokenType::Var,
                    pos: 0,
                    val: String::from("a"),
                },
                Token {
                    typ: TokenType::Op(Operator::Until),
                    pos: 2,
                    val: String::from("U"),
                },
                Token {
                    typ: TokenType::Par(Parenthesis::Open),
                    pos: 4,
                    val: String::from("("),
                },
                Token {
                    typ: TokenType::Var,
                    pos: 5,
                    val: String::from("b"),
                },
                Token {
                    typ: TokenType::Op(Operator::And),
                    pos: 7,
                    val: String::from("&"),
                },
                Token {
                    typ: TokenType::Var,
                    pos: 9,
                    val: String::from("c"),
                },
                Token {
                    typ: TokenType::Par(Parenthesis::Close),
                    pos: 10,
                    val: String::from(")"),
                },
            ]
        );

        let vec: Vec<_> = Lexer::new("xR~y".chars()).collect();

        assert_eq!(
            vec,
            vec![
                Token {
                    typ: TokenType::Var,
                    pos: 0,
                    val: String::from("x"),
                },
                Token {
                    typ: TokenType::Op(Operator::Release),
                    pos: 1,
                    val: String::from("R"),
                },
                Token {
                    typ: TokenType::Op(Operator::Not),
                    pos: 2,
                    val: String::from("~"),
                },
                Token {
                    typ: TokenType::Var,
                    pos: 3,
                    val: String::from("y"),
                },
            ]
        );

        let vec: Vec<_> = Lexer::new("A".chars()).collect();

        assert_eq!(
            vec,
            vec![Token {
                typ: TokenType::Unk,
                pos: 0,
                val: String::from("A"),
            }]
        );

        let vec: Vec<_> = Lexer::new("4abc".chars()).collect();

        assert_eq!(
            vec,
            vec![Token {
                typ: TokenType::Unk,
                pos: 0,
                val: String::from("4abc"),
            }]
        );

        let vec: Vec<_> = Lexer::new("(((24".chars()).collect();

        assert_eq!(
            vec,
            vec![
                Token {
                    typ: TokenType::Par(Parenthesis::Open),
                    pos: 0,
                    val: String::from("("),
                },
                Token {
                    typ: TokenType::Par(Parenthesis::Open),
                    pos: 1,
                    val: String::from("("),
                },
                Token {
                    typ: TokenType::Par(Parenthesis::Open),
                    pos: 2,
                    val: String::from("("),
                },
                Token {
                    typ: TokenType::Unk,
                    pos: 3,
                    val: String::from("24"),
                }
            ]
        );
    }
}

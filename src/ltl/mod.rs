use std::fmt;
use std::iter;

pub mod lexer;
pub mod parser;

#[derive(Clone, Debug, PartialEq)]
pub enum Ltl<AP> {
    True,
    False,
    Prop(AP),
    Not(Box<Self>),
    And(Box<Self>, Box<Self>),
    Or(Box<Self>, Box<Self>),
    Next(Box<Self>),
    Finally(Box<Self>),
    Globally(Box<Self>),
    Until(Box<Self>, Box<Self>),
    Release(Box<Self>, Box<Self>),
    WeakUntil(Box<Self>, Box<Self>),
    StrongRelease(Box<Self>, Box<Self>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LtlNNF<AP> {
    True,
    False,
    Prop(AP),
    NProp(AP),
    And(Box<Self>, Box<Self>),
    Or(Box<Self>, Box<Self>),
    Next(Box<Self>),
    Until(Box<Self>, Box<Self>),
    Release(Box<Self>, Box<Self>),
    WeakUntil(Box<Self>, Box<Self>),
    StrongRelease(Box<Self>, Box<Self>),
}

impl<AP> Ltl<AP> {
    pub fn not(x: Self) -> Self {
        Ltl::Not(Box::new(x))
    }

    pub fn and(x: Self, y: Self) -> Self {
        Ltl::And(Box::new(x), Box::new(y))
    }

    pub fn or(x: Self, y: Self) -> Self {
        Ltl::Or(Box::new(x), Box::new(y))
    }

    pub fn next(x: Self) -> Self {
        Ltl::Next(Box::new(x))
    }

    pub fn finally(x: Self) -> Self {
        Ltl::Finally(Box::new(x))
    }

    pub fn globally(x: Self) -> Self {
        Ltl::Globally(Box::new(x))
    }

    pub fn until(x: Self, y: Self) -> Self {
        Ltl::Until(Box::new(x), Box::new(y))
    }

    pub fn release(x: Self, y: Self) -> Self {
        Ltl::Release(Box::new(x), Box::new(y))
    }

    pub fn weak_until(x: Self, y: Self) -> Self {
        Ltl::WeakUntil(Box::new(x), Box::new(y))
    }

    pub fn strong_release(x: Self, y: Self) -> Self {
        Ltl::StrongRelease(Box::new(x), Box::new(y))
    }
}

impl<AP> LtlNNF<AP> {
    pub fn and(x: Self, y: Self) -> Self {
        LtlNNF::And(Box::new(x), Box::new(y))
    }

    pub fn or(x: Self, y: Self) -> Self {
        LtlNNF::Or(Box::new(x), Box::new(y))
    }

    pub fn next(x: Self) -> Self {
        LtlNNF::Next(Box::new(x))
    }

    pub fn finally(x: Self) -> Self {
        LtlNNF::Until(Box::new(LtlNNF::True), Box::new(x))
    }

    pub fn globally(x: Self) -> Self {
        LtlNNF::Release(Box::new(LtlNNF::False), Box::new(x))
    }

    pub fn until(x: Self, y: Self) -> Self {
        LtlNNF::Until(Box::new(x), Box::new(y))
    }

    pub fn release(x: Self, y: Self) -> Self {
        LtlNNF::Release(Box::new(x), Box::new(y))
    }

    pub fn weak_until(x: Self, y: Self) -> Self {
        LtlNNF::WeakUntil(Box::new(x), Box::new(y))
    }

    pub fn strong_release(x: Self, y: Self) -> Self {
        LtlNNF::StrongRelease(Box::new(x), Box::new(y))
    }
}

impl<AP> Ltl<AP> {
    pub fn subformulas<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Ltl<AP>> + 'a> {
        match self {
            Ltl::Not(x) | Ltl::Next(x) | Ltl::Finally(x) | Ltl::Globally(x) => {
                // chain subtree
                Box::new(iter::once(self).chain(x.subformulas()))
            }
            Ltl::And(x, y)
            | Ltl::Or(x, y)
            | Ltl::Until(x, y)
            | Ltl::Release(x, y)
            | Ltl::WeakUntil(x, y)
            | Ltl::StrongRelease(x, y) => {
                // chain both subtrees
                Box::new(
                    iter::once(self)
                        .chain(x.subformulas())
                        .chain(y.subformulas()),
                )
            }
            _ => Box::new(iter::once(self)),
        }
    }
}

impl<AP> LtlNNF<AP> {
    pub fn subformulas<'a>(&'a self) -> Box<dyn Iterator<Item = &'a LtlNNF<AP>> + 'a> {
        match self {
            LtlNNF::Next(x) => {
                // chain subtree
                Box::new(iter::once(self).chain(x.subformulas()))
            }
            LtlNNF::And(x, y)
            | LtlNNF::Or(x, y)
            | LtlNNF::Until(x, y)
            | LtlNNF::Release(x, y)
            | LtlNNF::WeakUntil(x, y)
            | LtlNNF::StrongRelease(x, y) => {
                // chain both subtrees
                Box::new(
                    iter::once(self)
                        .chain(x.subformulas())
                        .chain(y.subformulas()),
                )
            }
            _ => Box::new(iter::once(self)),
        }
    }
}

impl<AP: fmt::Display> fmt::Display for Ltl<AP> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ltl::True => write!(f, "true"),
            Ltl::False => write!(f, "false"),
            Ltl::Prop(a) => write!(f, "{}", a),
            Ltl::Not(x) => write!(f, "(¬ {})", x),
            Ltl::And(x, y) => write!(f, "({} ∧ {})", x, y),
            Ltl::Or(x, y) => write!(f, "({} ∨ {})", x, y),
            Ltl::Next(x) => write!(f, "(X {})", x),
            Ltl::Finally(x) => write!(f, "(F {})", x),
            Ltl::Globally(x) => write!(f, "(G {})", x),
            Ltl::Until(x, y) => write!(f, "({} U {})", x, y),
            Ltl::Release(x, y) => write!(f, "({} R {})", x, y),
            Ltl::WeakUntil(x, y) => write!(f, "({} W {})", x, y),
            Ltl::StrongRelease(x, y) => write!(f, "({} M {})", x, y),
        }
    }
}

impl<AP: fmt::Display> fmt::Display for LtlNNF<AP> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LtlNNF::True => write!(f, "true"),
            LtlNNF::False => write!(f, "false"),
            LtlNNF::Prop(a) => write!(f, "{}", a),
            LtlNNF::NProp(a) => write!(f, "¬ {}", a),
            LtlNNF::And(x, y) => write!(f, "({} ∧ {})", x, y),
            LtlNNF::Or(x, y) => write!(f, "({} ∨ {})", x, y),
            LtlNNF::Next(x) => write!(f, "(X {})", x),
            LtlNNF::Until(x, y) => write!(f, "({} U {})", x, y),
            LtlNNF::Release(x, y) => write!(f, "({} R {})", x, y),
            LtlNNF::WeakUntil(x, y) => write!(f, "({} W {})", x, y),
            LtlNNF::StrongRelease(x, y) => write!(f, "({} M {})", x, y),
        }
    }
}

impl<AP> Ltl<AP> {
    fn to_nnf_helper(self, b: bool) -> LtlNNF<AP> {
        match self {
            Ltl::True => {
                if b {
                    LtlNNF::False
                } else {
                    LtlNNF::True
                }
            }
            Ltl::False => {
                if b {
                    LtlNNF::True
                } else {
                    LtlNNF::False
                }
            }
            Ltl::Prop(a) => {
                if b {
                    LtlNNF::NProp(a)
                } else {
                    LtlNNF::Prop(a)
                }
            }
            Ltl::Not(x) => x.to_nnf_helper(!b),
            Ltl::And(x, y) => {
                if b {
                    LtlNNF::or(x.to_nnf_helper(b), y.to_nnf_helper(b))
                } else {
                    LtlNNF::and(x.to_nnf_helper(b), y.to_nnf_helper(b))
                }
            }
            Ltl::Or(x, y) => {
                if b {
                    LtlNNF::and(x.to_nnf_helper(b), y.to_nnf_helper(b))
                } else {
                    LtlNNF::or(x.to_nnf_helper(b), y.to_nnf_helper(b))
                }
            }
            Ltl::Next(x) => LtlNNF::next(x.to_nnf_helper(b)),
            Ltl::Finally(x) => {
                if b {
                    LtlNNF::globally(x.to_nnf_helper(b))
                } else {
                    LtlNNF::finally(x.to_nnf_helper(b))
                }
            }
            Ltl::Globally(x) => {
                if b {
                    LtlNNF::finally(x.to_nnf_helper(b))
                } else {
                    LtlNNF::globally(x.to_nnf_helper(b))
                }
            }
            Ltl::Until(x, y) => {
                if b {
                    LtlNNF::release(x.to_nnf_helper(b), y.to_nnf_helper(b))
                } else {
                    LtlNNF::until(x.to_nnf_helper(b), y.to_nnf_helper(b))
                }
            }
            Ltl::Release(x, y) => {
                if b {
                    LtlNNF::until(x.to_nnf_helper(b), y.to_nnf_helper(b))
                } else {
                    LtlNNF::release(x.to_nnf_helper(b), y.to_nnf_helper(b))
                }
            }
            Ltl::WeakUntil(x, y) => {
                if b {
                    LtlNNF::strong_release(x.to_nnf_helper(b), y.to_nnf_helper(b))
                } else {
                    LtlNNF::weak_until(x.to_nnf_helper(b), y.to_nnf_helper(b))
                }
            }
            Ltl::StrongRelease(x, y) => {
                if b {
                    LtlNNF::weak_until(x.to_nnf_helper(b), y.to_nnf_helper(b))
                } else {
                    LtlNNF::strong_release(x.to_nnf_helper(b), y.to_nnf_helper(b))
                }
            }
        }
    }

    pub fn to_nnf(self) -> LtlNNF<AP> {
        self.to_nnf_helper(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ltl_fmt() {
        assert_eq!("true", format!("{}", Ltl::<i32>::True));
        assert_eq!("false", format!("{}", Ltl::<i32>::False));

        let ltl = Ltl::and(Ltl::Prop(1), Ltl::Prop(2));

        assert_eq!("(1 ∧ 2)", format!("{}", ltl));

        let ltl = Ltl::until(Ltl::Prop(1), Ltl::release(Ltl::Prop(2), Ltl::Prop(3)));

        assert_eq!("(1 U (2 R 3))", format!("{}", ltl));

        let ltl = Ltl::finally(Ltl::next(Ltl::not(Ltl::Prop(3))));

        assert_eq!("(F (X (¬ 3)))", format!("{}", ltl));
    }

    #[test]
    fn ltl_subformulas() {
        assert_eq!(
            vec![&Ltl::<i32>::True],
            Ltl::True.subformulas().collect::<Vec<_>>()
        );

        assert_eq!(
            vec![&Ltl::Prop(1)],
            Ltl::Prop(1).subformulas().collect::<Vec<_>>()
        );

        assert_eq!(
            vec![&Ltl::not(Ltl::Prop(1)), &Ltl::Prop(1)],
            Ltl::not(Ltl::Prop(1)).subformulas().collect::<Vec<_>>()
        );

        let p = Ltl::and(Ltl::Prop(1), Ltl::Prop(2));

        assert_eq!(
            vec![&p, &Ltl::Prop(1), &Ltl::Prop(2)],
            p.subformulas().collect::<Vec<_>>()
        );

        let p = Ltl::until(Ltl::Prop(1), Ltl::Prop(2));
        let q = Ltl::next(p.clone());

        assert_eq!(
            vec![&q, &p, &Ltl::Prop(1), &Ltl::Prop(2)],
            q.subformulas().collect::<Vec<_>>()
        );
    }

    #[test]
    fn ltl_nnf_subformulas() {
        // TODO
    }
}

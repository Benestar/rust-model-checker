use std::fmt;

pub mod lexer;
pub mod parser;

#[derive(Debug)]
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
}

#[derive(Debug)]
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
                let xn = Box::new(x.to_nnf_helper(b));
                let yn = Box::new(y.to_nnf_helper(b));

                if b {
                    LtlNNF::Or(xn, yn)
                } else {
                    LtlNNF::And(xn, yn)
                }
            }
            Ltl::Or(x, y) => {
                let xn = Box::new(x.to_nnf_helper(b));
                let yn = Box::new(y.to_nnf_helper(b));

                if b {
                    LtlNNF::And(xn, yn)
                } else {
                    LtlNNF::Or(xn, yn)
                }
            }
            Ltl::Next(x) => LtlNNF::Next(Box::new(x.to_nnf_helper(b))),
            Ltl::Finally(x) => {
                let xn = Box::new(x.to_nnf_helper(b));

                if b {
                    LtlNNF::globally(xn)
                } else {
                    LtlNNF::finally(xn)
                }
            }
            Ltl::Globally(x) => {
                let xn = Box::new(x.to_nnf_helper(b));

                if b {
                    LtlNNF::finally(xn)
                } else {
                    LtlNNF::globally(xn)
                }
            }
            Ltl::Until(x, y) => {
                let xn = Box::new(x.to_nnf_helper(b));
                let yn = Box::new(y.to_nnf_helper(b));

                if b {
                    LtlNNF::Release(xn, yn)
                } else {
                    LtlNNF::Until(xn, yn)
                }
            }
            Ltl::Release(x, y) => {
                let xn = Box::new(x.to_nnf_helper(b));
                let yn = Box::new(y.to_nnf_helper(b));

                if b {
                    LtlNNF::Until(xn, yn)
                } else {
                    LtlNNF::Release(xn, yn)
                }
            }
        }
    }

    pub fn to_nnf(self) -> LtlNNF<AP> {
        self.to_nnf_helper(false)
    }
}

impl<AP> LtlNNF<AP> {
    pub fn finally(x: Box<Self>) -> Self {
        LtlNNF::Until(Box::new(LtlNNF::True), x)
    }

    pub fn globally(x: Box<Self>) -> Self {
        LtlNNF::Release(Box::new(LtlNNF::False), x)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ltl_fmt() {
        assert_eq!("true", format!("{}", Ltl::<u32>::True));
        assert_eq!("false", format!("{}", Ltl::<u32>::False));

        let ltl = Ltl::And(Box::new(Ltl::Prop(1)), Box::new(Ltl::Prop(2)));

        assert_eq!("(1 ∧ 2)", format!("{}", ltl));

        let ltl = Ltl::Until(
            Box::new(Ltl::Prop(1)),
            Box::new(Ltl::Release(Box::new(Ltl::Prop(2)), Box::new(Ltl::Prop(3)))),
        );

        assert_eq!("(1 U (2 R 3))", format!("{}", ltl));

        let ltl = Ltl::Finally(Box::new(Ltl::Next(Box::new(Ltl::Not(Box::new(
            Ltl::Prop(3),
        ))))));

        assert_eq!("(F (X (¬ 3)))", format!("{}", ltl));
    }
}

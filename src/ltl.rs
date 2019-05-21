use std::fmt;

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
pub enum Ltln<AP> {
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

impl<AP: fmt::Display> fmt::Display for Ltln<AP> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ltln::True => write!(f, "true"),
            Ltln::False => write!(f, "false"),
            Ltln::Prop(a) => write!(f, "{}", a),
            Ltln::NProp(a) => write!(f, "¬ {}", a),
            Ltln::And(x, y) => write!(f, "({} ∧ {})", x, y),
            Ltln::Or(x, y) => write!(f, "({} ∨ {})", x, y),
            Ltln::Next(x) => write!(f, "(X {})", x),
            Ltln::Until(x, y) => write!(f, "({} U {})", x, y),
            Ltln::Release(x, y) => write!(f, "({} R {})", x, y),
        }
    }
}

impl<AP> Ltl<AP> {
    fn to_ltln_helper(self, b: bool) -> Ltln<AP> {
        match self {
            Ltl::True => if b { Ltln::True } else { Ltln::False },
            Ltl::False => if b { Ltln::False } else { Ltln::True },
            Ltl::Prop(a) => if b { Ltln::Prop(a) } else { Ltln::NProp(a) },
            Ltl::Not(x) => x.to_ltln_helper(!b),
            Ltl::And(x, y) => {
                let xn = Box::new(x.to_ltln_helper(b));
                let yn = Box::new(y.to_ltln_helper(b));

                if b { Ltln::And(xn, yn) } else { Ltln::Or(xn, yn) }
            },
            Ltl::Or(x, y) => {
                let xn = Box::new(x.to_ltln_helper(b));
                let yn = Box::new(y.to_ltln_helper(b));

                if b { Ltln::Or(xn, yn) } else { Ltln::And(xn, yn) }
            },
            Ltl::Next(x) => Ltln::Next(Box::new(x.to_ltln_helper(b))),
            Ltl::Finally(x) => {
                let xn = Box::new(x.to_ltln_helper(b));

                if b {
                    Ltln::Until(Box::new(Ltln::True), xn)
                } else {
                    Ltln::Release(Box::new(Ltln::False), xn)
                }
            },
            Ltl::Globally(x) => {
                let xn = Box::new(x.to_ltln_helper(b));

                if b {
                    Ltln::Release(Box::new(Ltln::False), xn)
                } else {
                    Ltln::Until(Box::new(Ltln::True), xn)
                }
            },
            Ltl::Until(x, y) => {
                let xn = Box::new(x.to_ltln_helper(b));
                let yn = Box::new(y.to_ltln_helper(b));

                if b { Ltln::Until(xn, yn) } else { Ltln::Release(xn, yn) }
            },
            Ltl::Release(x, y) => {
                let xn = Box::new(x.to_ltln_helper(b));
                let yn = Box::new(y.to_ltln_helper(b));

                if b { Ltln::Release(xn, yn) } else { Ltln::Until(xn, yn) }
            },
        }
    }

    pub fn to_ltln(self) -> Ltln<AP> {
        self.to_ltln_helper(false)
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
            Box::new(Ltl::Release(Box::new(Ltl::Prop(2)), Box::new(Ltl::Prop(3))))
        );

        assert_eq!("(1 U (2 R 3))", format!("{}", ltl));

        let ltl = Ltl::Finally(Box::new(Ltl::Next(Box::new(Ltl::Not(Box::new(Ltl::Prop(3)))))));

        assert_eq!("(F (X (¬ 3)))", format!("{}", ltl));
    }
}

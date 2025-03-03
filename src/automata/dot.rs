use crate::automata::explicit::ExplicitTransitionSystem;
use std::fmt;
use std::fmt::{Error, Formatter};

impl<State: fmt::Display, Label: fmt::Display> fmt::Display for ExplicitTransitionSystem<State, Label> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "digraph {{\n")?;
        write!(f, "  rankdir=LR;\n")?;
        write!(f, "  node [shape=circle];\n")?;

        for (q, l, p) in self.trans.iter() {
            write!(f, "  {} -> {} [label=\"{}\"];\n", q, p, l)?;
        }

        for s in self.init.iter() {
            write!(f, "  init{} [label=\"\", shape=point];\n", s)?;
            write!(f, "  init{} -> {};\n", s, s)?;
        }

        write!(f, "}}\n")?;

        Ok(())
    }
}

use std::collections::HashSet;

pub trait Automaton {
    type State;
    type Label;

    /// The initial states of this automaton
    fn init(&self) -> Box<dyn Iterator<Item = Self::State>>;

    /// The transition function
    fn trans(
        &self,
        state: Self::State,
        label: Self::Label,
    ) -> Box<dyn Iterator<Item = Self::State>>;

    /// The accepting predicate on states
    fn acc(&self, state: Self::State) -> bool;
}

pub struct ExplicitAutomaton<State, Label> {
    init: HashSet<State>,
    trans: HashSet<(State, Label, State)>,
    acc: HashSet<State>,
}

//! # Explicit Automata Module
//!
//! This module provides explicit-state representations of automata, including transition systems,
//! Büchi automata, and Rabin automata. It is designed to work with formal verification and model-checking
//! tasks where automata are explicitly constructed, stored, and manipulated.
//!
//! ## Overview
//!
//! Automata are foundational computational models used to handle infinite-state systems, temporal logic,
//! and formal methods. The `explicit` module defines concrete structures for representing transition systems
//! and their acceptance conditions explicitly, making it suitable for algorithms that require a direct
//! analysis or manipulation of state-space representations.
//!
//! The key components of this module are:
//!
//! - [`ExplicitTransitionSystem`]: A concrete representation of a transition system, where transitions,
//!   states, and initial states are explicitly stored in sets.
//!
//! - [`ExplicitBuechiAcceptance`]: Encodes the set of accepting states for a Büchi automaton.
//!
//! - [`ExplicitRabinAcceptance`]: Encodes infinite (`infs`) and finite (`fins`) acceptance sets for a Rabin automaton.
//!
//! - [`ExplicitBuechiAutomaton`]: Combines an explicit transition system and a Büchi acceptance condition
//!   to define a complete Büchi automaton.
//!
//! - [`ExplicitRabinAutomaton`]: Combines an explicit transition system and a Rabin acceptance condition
//!   to define a complete Rabin automaton.
//!
//! ## Explicit Representations
//!
//! When dealing with automata, there are implicit and explicit representations:
//!
//! - Implicit representations (such as those defined by traits like [`TransitionSystem`] and
//!   [`BuechiAcceptance`]) define automata abstractly, where states and transitions are lazily evaluated.
//! - Explicit representations, like the ones in this module, store all states and transitions explicitly
//!   in sets, making them easier to analyze but also more memory-intensive.
//!
//! ## Features
//!
//! This module provides:
//!
//! - An explicit way to store transitions, states, and acceptance information.
//! - Utilities to convert implicit automata (e.g., those defined using traits) into explicit representations
//!   via methods like [`ExplicitTransitionSystem::from_transition_system`] and
//!   [`ExplicitBuechiAcceptance::from_buechi_acceptance`].
//!
//! ## Use Cases
//!
//! Explicit-state representations are particularly useful for:
//!
//! - **State-space exploration:** When we need to enumerate all possible states and transitions
//!   explicitly, for example, as part of a search algorithm.
//! - **Formal verification:** When analyzing automata to check properties such as language inclusion or emptiness.
//! - **Automata-based algorithms:** E.g., constructing automata from logical formulas and converting between
//!   different types of automata.
//!
//! ## Structure of the Explicit Automata
//!
//! The `explicit` module provides the following components:
//!
//! ### 1. Transition Systems
//! - [`ExplicitTransitionSystem`]: Stores the set of states, transitions, and initial states explicitly.
//!
//! ### 2. Acceptance Conditions
//! - [`ExplicitBuechiAcceptance`]: Stores the Büchi acceptance condition using a set of accepting states.
//! - [`ExplicitRabinAcceptance`]: Stores the Rabin acceptance condition using `infs` (infinite sets) and
//!   `fins` (finite sets).
//!
//! ### 3. Complete Automata
//! - [`ExplicitBuechiAutomaton`]: Combines a transition system and Büchi acceptance.
//! - [`ExplicitRabinAutomaton`]: Combines a transition system and Rabin acceptance.
//!
//! ## Limitations
//!
//! Explicit representations are powerful but memory-intensive. Large automata with many states and transitions
//! can result in excessive memory usage, so implicit representations may be preferred for certain scenarios.
//!

use crate::automata::{BuechiAutomaton, RabinAutomaton, TransitionSystem};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;

pub struct ExplicitTransitionSystem<State, Label> {
    pub(crate) init: HashSet<State>,
    pub(crate) trans: HashSet<(State, Label, State)>,
}

impl<State, Label> ExplicitTransitionSystem<State, Label> {
    pub fn new(init: HashSet<State>, trans: HashSet<(State, Label, State)>) -> Self {
        Self { init, trans }
    }
}

impl<State: Clone + Eq, Label: Eq> TransitionSystem for ExplicitTransitionSystem<State, Label> {
    type State = State;
    type Label = Label;

    fn init(&self) -> impl Iterator<Item = Self::State> {
        self.init.iter().cloned()
    }

    fn trans(&self, state: &Self::State, label: &Self::Label) -> impl Iterator<Item=Self::State> {
        self.trans.iter()
            .filter(move |(s, l, _)| s == state && l == label)
            .map(|(_, _, s)| s.clone())
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ExplicitState(usize);

impl fmt::Display for ExplicitState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct StateSet<State> {
    indices: HashMap<State, ExplicitState>,
}

impl<State: Eq + Hash> StateSet<State> {
    pub fn new() -> Self {
        Self { indices: HashMap::new() }
    }

    pub fn contains(&self, t: &State) -> bool {
        self.indices.contains_key(t)
    }

    pub fn get_or_insert(&mut self, t: State) -> ExplicitState {
        let len = ExplicitState(self.indices.len());

        *self.indices.entry(t).or_insert(len)
    }

    pub fn get(&self, t: &State) -> Option<ExplicitState> {
        self.indices.get(t).cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&State, &ExplicitState)> {
        self.indices.iter()
    }

    pub fn filter<F: Fn(&State) -> bool>(&self, f: F) -> impl Iterator<Item = ExplicitState> + use<'_, State, F> {
        self.indices.iter().filter(move |(t, _)| f(t)).map(|(_, s)| s.clone())
    }

    pub fn states(&self) -> impl Iterator<Item = ExplicitState> + use<'_, State> {
        self.indices.values().cloned()
    }
}

pub trait ExploreTransitionSystem<Label, State> {
    fn explore<'a>(&'a self, alphabet: &'a [Label]) -> (ExplicitTransitionSystem<ExplicitState, &'a Label>, StateSet<State>);
}

impl<T: TransitionSystem> ExploreTransitionSystem<T::Label, T::State> for T
where
    T::State: Clone + Eq + Hash,
    T::Label: Eq + Hash,
{
    fn explore<'a>(&'a self, alphabet: &'a [T::Label]) -> (ExplicitTransitionSystem<ExplicitState, &'a T::Label>, StateSet<T::State>) {
        let mut states = StateSet::new();
        let mut init = HashSet::new();
        let mut trans = HashSet::new();
        let mut stack = Vec::new();

        for s in self.init() {
            stack.push(s.clone());
            init.insert(states.get_or_insert(s));
        }

        while let Some(s) = stack.pop() {
            let idx = states.get_or_insert(s.clone());
            for l in alphabet {
                for t in self.trans(&s, l) {
                    if !states.contains(&t) {
                        stack.push(t.clone());
                    }
                    trans.insert((idx, l, states.get_or_insert(t)));
                }
            }
        }

        let ets = ExplicitTransitionSystem { init, trans };

        (ets, states)
    }
}

pub struct ExplicitBuechiAcceptance<State> {
    acc: HashSet<State>,
}

pub struct ExplicitRabinAcceptance<State> {
    infs: HashSet<State>,
    fins: HashSet<State>,
}

pub struct ExplicitBuechiAutomaton<State, Label> {
    pub(crate) trans: ExplicitTransitionSystem<State, Label>,
    pub(crate) acc: ExplicitBuechiAcceptance<State>,
}

pub struct ExplicitRabinAutomaton<State, Label> {
    pub(crate) trans: ExplicitTransitionSystem<State, Label>,
    pub(crate) acc: ExplicitRabinAcceptance<State>,
}


pub trait ExploreBuechi<Label> {

    fn explore<'a>(&'a self, alphabet: &'a [Label]) -> ExplicitBuechiAutomaton<ExplicitState, &'a Label>;
}
impl<T: BuechiAutomaton> ExploreBuechi<T::Label> for T
where
    T::State: Clone + Eq + Hash,
    T::Label: Eq + Hash,
{

    fn explore<'a>(&'a self, alphabet: &'a [T::Label]) -> ExplicitBuechiAutomaton<ExplicitState, &'a T::Label> {
        let (trans, states) = ExploreTransitionSystem::explore(self, alphabet);

        let acc = ExplicitBuechiAcceptance {
            acc: states.filter(|s| self.acc(s)).collect()
        };

        ExplicitBuechiAutomaton { trans, acc }
    }
}

pub trait ExploreRabin<Label> {

    fn explore<'a>(&'a self, alphabet: &'a [Label]) -> ExplicitRabinAutomaton<ExplicitState, &'a Label>;
}

impl<T: RabinAutomaton> ExploreRabin<T::Label> for T
where
    T::State: Clone + Eq + Hash,
    T::Label: Eq + Hash,
{

    fn explore<'a>(&'a self, alphabet: &'a [T::Label]) -> ExplicitRabinAutomaton<ExplicitState, &'a T::Label> {
        let (trans, states) = ExploreTransitionSystem::explore(self, alphabet);

        let acc = ExplicitRabinAcceptance {
            infs: states.filter(|s| self.infs(s)).collect(),
            fins: states.filter(|s| self.fins(s)).collect()
        };

        ExplicitRabinAutomaton { trans, acc }
    }
}

//! # Traits Documentation
//!
//! This module provides an abstract definition of transition systems and automata,
//! including Büchi and Rabin automata, along with their associated acceptance conditions.
//! These traits define the foundation for constructing and analyzing automata with specific
//! acceptance criteria.
//!
//! ## Traits
//!
//! ### `TransitionSystem`
//!
//! The `TransitionSystem` trait defines the basic structure of a transition
//! system, which is the foundation for automata. It provides methods for retrieving
//! the initial states and representing the transition relation.
//!
//! #### Associated Types
//! - `State`: The type representing the states of the transition system.
//! - `Label`: The type representing the labels on the transitions.
//!
//! #### Required Methods
//! - `fn init(&self) -> impl Iterator<Item = Self::State>`
//!   Returns an iterator over the initial states of the transition system.
//!
//! - `fn trans(&self, state: Self::State, label: Self::Label) -> impl Iterator<Item = Self::State>`
//!   Represents the transition relation of the system. Given a state and a transition
//!   label, it returns an iterator over the set of successor states.
//!
//! ### `BuechiAcceptance`
//!
//! The `BuechiAcceptance` trait defines the Büchi acceptance condition in terms
//! of accepting states.
//!
//! #### Associated Types
//! - `State`: The type representing the states of the automaton.
//!
//! #### Required Methods
//! - `fn acc(&self, state: &Self::State) -> bool`
//!   Determines if a state is an accepting state. Returns `true` if it is and `false` otherwise.
//!
//! ### `RabinAcceptance`
//!
//! The `RabinAcceptance` trait defines the Rabin acceptance condition in terms of
//! two mutually exclusive conditions: the *infinite* and *finite* sets of states.
//!
//! #### Associated Types
//! - `State`: The type representing the states of the automaton.
//!
//! #### Required Methods
//! - `fn infs(&self, state: &Self::State) -> bool`
//!   Determines if a state belongs to the infinite set. Returns `true` if it does.
//!
//! - `fn fins(&self, state: &Self::State) -> bool`
//!   Determines if a state belongs to the finite set. Returns `true` if it does.
//!
//! ### `BuechiAutomaton`
//!
//! The `BuechiAutomaton` trait combines a `TransitionSystem` with a Büchi acceptance condition
//! to define a Büchi automaton.
//!
//! #### Supertraits
//! - [`TransitionSystem`]
//! - [`BuechiAcceptance`]
//!
//! #### Notes
//! Any type that implements both `TransitionSystem` and `BuechiAcceptance` automatically
//! implements `BuechiAutomaton` via the provided blanket implementation.
//!
//! ### `RabinAutomaton`
//!
//! The `RabinAutomaton` trait combines a `TransitionSystem` with a Rabin acceptance condition
//! to define a Rabin automaton.
//!
//! #### Supertraits
//! - [`TransitionSystem`]
//! - [`RabinAcceptance`]
//!
//! #### Notes
//! Any type that implements both `TransitionSystem` and `RabinAcceptance` automatically
//! implements `RabinAutomaton` via the provided blanket implementation.
//!
//! ## Summary
//!
//! This module provides an abstract definition of transition systems and automata, including
//! Büchi and Rabin automata, along with their associated acceptance conditions. These traits
//! are designed to be composable and extensible for constructing concrete automata models.
//!

pub mod explicit;
pub mod hoaf;
pub mod dot;

/// Represents a transition system, which is the foundation for an automaton.
///
/// A transition system consists of a set of initial states and a transition function.
///
/// Associated types:
/// - `State`: Represents the states of the system.
/// - `Label`: Represents the labels on transitions.
///
/// # Examples
///
/// ```
/// use std::collections::HashSet;
/// use rust_model_checker::automata::TransitionSystem;
///
/// struct MyTransitionSystem;
///
/// impl TransitionSystem for MyTransitionSystem {
///     type State = isize;
///     type Label = char;
///
///     fn init(&self) -> impl Iterator<Item = Self::State> {
///         vec![0].into_iter()
///     }
///
///     fn trans(&self, state: &Self::State, label: &Self::Label) -> impl Iterator<Item = Self::State> {
///         match label {
///             '+' => vec![state + 1].into_iter(),
///             '-' => vec![state - 1].into_iter(),
///             _ => vec![].into_iter()
///         }
///     }
/// }
///
/// let ts = MyTransitionSystem;
///
/// let init_states: Vec<isize> = ts.init().collect();
/// assert_eq!(init_states, vec![0]);
///
/// let plus_states: Vec<isize> = ts.trans(&0, &'+').collect();
/// assert_eq!(plus_states, vec![1]);
///
/// let minus_states: Vec<isize> = ts.trans(&5, &'-').collect();
/// assert_eq!(minus_states, vec![4]);
///
/// let empty: Vec<isize> = ts.trans(&3, &'x').collect();
/// assert!(empty.is_empty());
/// ```
///
/// # See Also
/// - [`BuechiAutomaton`]: Defines a transition system with Büchi acceptance condition.
/// - [`RabinAutomaton`]: Defines a transition system with Rabin acceptance condition.
pub trait TransitionSystem {
    type State;
    type Label;

    /// The initial states of this automaton
    fn init(&self) -> impl Iterator<Item = Self::State>;

    /// The transition function
    fn trans(
        &self,
        state: &Self::State,
        label: &Self::Label,
    ) -> impl Iterator<Item = Self::State>;
}

/// A Büchi acceptance condition consists of a function
/// that determines whether a state is an accepting state.
pub trait BuechiAutomaton : TransitionSystem {

    fn acc(&self, state: &Self::State) -> bool;
}

/// A Rabin acceptance condition consists of two functions
/// that determine which states are infinite and which are finite states.
pub trait RabinAutomaton : TransitionSystem {

    fn infs(&self, state: &Self::State) -> bool;

    fn fins(&self, state: &Self::State) -> bool;
}

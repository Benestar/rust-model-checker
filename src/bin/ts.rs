use rust_model_checker::automata::explicit::ExploreTransitionSystem;
use rust_model_checker::automata::TransitionSystem;

struct MyTransitionSystem {}

impl TransitionSystem for MyTransitionSystem {
    type State = isize;
    type Label = char;

    fn init(&self) -> impl Iterator<Item=Self::State> {
        vec![0].into_iter()
    }

    fn trans(&self, state: &Self::State, label: &Self::Label) -> impl Iterator<Item=Self::State> {
        match label {
            '+' if state < &3 => vec![state + 1].into_iter(),
            '-' if state > &-3 => vec![state - 1].into_iter(),
            _ => vec![].into_iter()
        }
    }
}

fn main() {
    let ts = MyTransitionSystem {};
    let alphabet = vec!['+', '-'];

    let (ets, _) = ts.explore(&alphabet);

    println!("{}", ets)
}
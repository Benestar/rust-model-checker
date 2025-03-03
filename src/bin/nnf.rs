extern crate rust_model_checker;

use rust_model_checker::ltl;
use std::env;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Usage: ./nnf <formula>");

        process::exit(1);
    }

    ltl::parser::parse(&args[1]).map_or_else(
        |error| eprintln!("Error: {}", error),
        |formula| println!("{}", formula.to_nnf()),
    );
}

extern crate rust_model_checker;

use rust_model_checker::buechi::NGBA;
use rust_model_checker::ltl::parser;
use rust_model_checker::ltl::LtlNNF;
use rust_model_checker::ltl_to_buechi::{LtlToNGBA, SimpleLtlToNGBA};
use std::collections::BTreeSet;

use std::collections::HashSet;
use std::env;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Usage: ./rust_model_checker <ltl>");
        process::exit(1);
    }

    let ltl = parser::parse_ltl(&args[1]).to_nnf();

    if ltl == LtlNNF::False {
        print_false();
        process::exit(0);
    }

    let ngba = SimpleLtlToNGBA.to_nba(&ltl);

    let alphabet = ltl
        .subformulas()
        .filter_map(|s| match s {
            LtlNNF::Prop(a) | LtlNNF::NProp(a) => Some(a.clone()),
            _ => None,
        })
        .collect::<HashSet<_>>();

    let alphabet = alphabet.iter().collect::<Vec<_>>();

    let mut pppppp = Vec::new();

    for bits in 0..2usize.pow(alphabet.len() as u32) {
        let mut set = BTreeSet::new();

        for (i, &a) in alphabet.iter().enumerate() {
            if (bits >> i) % 2 == 1 {
                set.insert(a.clone());
            }
        }

        pppppp.push(set);
    }

    let (states, engba) = ngba.explore(&pppppp);

    println!("HOA: v1");
    for init in engba.initial {
        println!("Start: {}", init);
    }
    println!(
        "AP: {} {}",
        alphabet.len(),
        alphabet
            .iter()
            .map(|a| format!("\"{}\"", a))
            .collect::<Vec<_>>()
            .join(" ")
    );

    if engba.accepting.len() > 0 {
        println!(
            "Acceptance: {} {}",
            engba.accepting.len(),
            engba
                .accepting
                .iter()
                .enumerate()
                .map(|(i, _)| format!("Inf({})", i))
                .collect::<Vec<_>>()
                .join("&")
        );
    } else {
        println!("Acceptance: 0 t");
    }

    println!("--BODY--");

    let mut trans = engba.trans.iter().collect::<Vec<_>>();
    trans.sort();

    let mut last = 0;

    println!(
        "State: 0 \"{{{}}}\" {{{}}}",
        states[0].iter().map(|f| f.to_string()).collect::<Vec<_>>().join(", "),
        engba
            .accepting
            .iter()
            .enumerate()
            .filter_map(|(i, a)| if a.contains(&0) {
                Some(i.to_string())
            } else {
                None
            })
            .collect::<Vec<_>>()
            .join(" ")
    );

    for (p, s, q) in trans {
        if *p != last {
            if *p > last {
                for p in last + 1..*p {
                    println!(
                        "State: {} \"{{{}}}\" {{{}}}",
                        p,
                        states[p].iter().map(|f| f.to_string()).collect::<Vec<_>>().join(", "),
                        engba
                            .accepting
                            .iter()
                            .enumerate()
                            .filter_map(|(i, a)| if a.contains(&p) {
                                Some(i.to_string())
                            } else {
                                None
                            })
                            .collect::<Vec<_>>()
                            .join(" ")
                    );
                }
            }

            last = *p;

            println!(
                "State: {} \"{{{}}}\" {{{}}}",
                p,
                states[*p].iter().map(|f| f.to_string()).collect::<Vec<_>>().join(", "),
                engba
                    .accepting
                    .iter()
                    .enumerate()
                    .filter_map(|(i, a)| if a.contains(p) {
                        Some(i.to_string())
                    } else {
                        None
                    })
                    .collect::<Vec<_>>()
                    .join(" ")
            );
        }

        let cond = if alphabet.is_empty() {
            String::from("t")
        } else {
            alphabet
                .iter()
                .enumerate()
                .map(|(i, &a)| if s.contains(a) {
                    i.to_string()
                } else {
                    format!("!{}", i)
                })
                .collect::<Vec<_>>()
                .join("&")
        };

        println!(
            "[{}] {}",
            cond,
            q
        );
    }

    for p in last+1..states.len() {
        println!(
            "State: {} \"{{{}}}\" {{{}}}",
            p,
            states[p].iter().map(|f| f.to_string()).collect::<Vec<_>>().join(", "),
            engba
                .accepting
                .iter()
                .enumerate()
                .filter_map(|(i, a)| if a.contains(&p) {
                    Some(i.to_string())
                } else {
                    None
                })
                .collect::<Vec<_>>()
                .join(" ")
        );
    }

    println!("--END--");
}

fn print_false() {
    println!("HOA: v1");
    println!("AP: 0");
    println!("Acceptance: 0 f");
    println!("--BODY--");
    println!("--END--");
}

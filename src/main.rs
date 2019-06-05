extern crate rust_model_checker;

use std::collections::BTreeSet;
use rust_model_checker::ltl_to_buechi::{LtlToNGBA, SimpleLtlToNGBA};
use rust_model_checker::ltl::LtlNNF;
use rust_model_checker::buechi::NGBA;

fn main() {
    let ltl = LtlNNF::until(LtlNNF::Prop("a"), LtlNNF::Prop("b"));

    let ngba = SimpleLtlToNGBA.to_nba(&ltl);

    let mut s1 = BTreeSet::new();
    let mut s2 = BTreeSet::new();
    let mut s3 = BTreeSet::new();
    let mut s4 = BTreeSet::new();

    s2.insert("a");
    s3.insert("b");
    s4.insert("a");
    s4.insert("b");

    let alphabet = vec!["a", "b"];

    let engba = ngba.explore(&vec![s1, s2, s3, s4]);

    println!("HOA: v1");
    for init in engba.initial {
        println!("Start: {}", init);
    }
    println!("AP: {} {}", alphabet.len(), alphabet.iter().map(|a| format!("\"{}\"", a)).collect::<Vec<_>>().join(" "));

    if engba.accepting.len() > 0 {
        println!("Acceptance: {} {}", engba.accepting.len(), engba.accepting.iter().enumerate().map(|(i, _)| format!("Inf({})", i)).collect::<Vec<_>>().join("&"));
    } else {
        println!("Acceptance: 0 t");
    }

    println!("--BODY--");

    let mut trans = engba.trans.iter().collect::<Vec<_>>();
    trans.sort();

    let mut last = 0;


    println!("State: 0 {{{}}}", engba.accepting.iter().enumerate().filter_map(|(i, a)| if a.contains(&0) { Some(i.to_string()) } else { None }).collect::<Vec<_>>().join(" "));

    for (p, s, q) in trans {
        if *p != last {
            if *p > last {
                for i in last+1..*p {
                    println!("State: {} {{}}", i);
                }
            }

            last = *p;

            println!("State: {} {{{}}}", p, engba.accepting.iter().enumerate().filter_map(|(i, a)| if a.contains(p) { Some(i.to_string()) } else { None }).collect::<Vec<_>>().join(" "));
        }

        println!("[{}] {}", alphabet.iter().enumerate().map(|(i, a)| if s.contains(a) { i.to_string() } else { format!("!{}", i) } ).collect::<Vec<_>>().join("&"),q);
    }

    for i in last+1..last+5 {
        println!("State: {} {{}}", i);
    }

    println!("--END--");
}

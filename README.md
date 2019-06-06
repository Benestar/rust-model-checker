# A Model Checker in Rust

After installing Rust and Cargo, one can execute `cargo run -- "<formula>" to obtain a generalized Buechi automaton.

The following command renders the resulting automaton to the file `ngba.pdf`:

```
cargo run -- "<formula>" 2>/dev/null | autfilt --dot=Bar | dot -Tpdf > ngba.pdf
```

The following command tests some random ltl formulas against the tool `ltl2tgba` from spot:

```
randltl -n 5 -r --seed $(shuf -i 0-65000 -n 1) a b | ltlcross "ltl2tgba -s %f >%O" "./target/debug/rust-model-checker %f 2>/dev/null > %O" 
```

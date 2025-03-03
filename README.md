# A Model Checker in Rust

Work in progress

## Test Scripts

Parsing LTL and calculating negation-normal form:

```
cargo run --bin nnf "~ (a & (b U c))"
```

Explicit representation of an automaton (requires installation of `dot` and `feh`):

```
cargo run --bin ts | dot -Tpng | feh -
```

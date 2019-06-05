use rust_model_checker::ltl::parser;

fn main() {
    print!("{:?}", parser::parse_ltl("NOT true || ff"));
}

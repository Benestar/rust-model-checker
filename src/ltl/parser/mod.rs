use super::Ltl;

lalrpop_mod!(ltl_parser, "/ltl/parser/ltl_parser.rs");
pub enum UnOpcode {
    Not,
    Finally,
    Globally,
    Next,
}

pub enum BinOpcode {
    Imp,
    Biimp,
    Xor,
    And,
    Or,
    Until,
    WUntil,
    Release,
    SRelease,
}

pub fn un_op<AP>(op: UnOpcode, expr: Box<Ltl<AP>>) -> Ltl<AP> {
    match op {
        UnOpcode::Not => Ltl::Not(expr),
        UnOpcode::Next => Ltl::Next(expr),
        UnOpcode::Globally => Ltl::Globally(expr),
        UnOpcode::Finally => Ltl::Finally(expr),
    }
}

pub fn bin_op<AP: Clone>(l: Box<Ltl<AP>>, op: BinOpcode, r: Box<Ltl<AP>>) -> Ltl<AP> {
    match op {
        BinOpcode::And => Ltl::And(l, r),
        BinOpcode::Or => Ltl::Or(l, r),
        BinOpcode::Release => Ltl::Release(l, r),
        BinOpcode::SRelease => Ltl::Until(r.clone(), Box::new(Ltl::And(l, r))),
        BinOpcode::Until => Ltl::Until(l, r),
        BinOpcode::WUntil => Ltl::Release(r.clone(), Box::new(Ltl::Or(r, l))),
        BinOpcode::Xor => Ltl::and(Ltl::Or(l.clone(), r.clone()), Ltl::not(Ltl::Or(l, r))),
        BinOpcode::Imp => Ltl::Or(Box::new(Ltl::Not(l)), r),
        BinOpcode::Biimp => Ltl::or(
            Ltl::And(l.clone(), r.clone()),
            Ltl::and(Ltl::Not(l), Ltl::Not(r)),
        ),
    }
}

pub fn parse_ltl(s: &str) -> Ltl<String> {
    ltl_parser::FormulaParser::new().parse(s).unwrap()
}

#[test]
fn ltl_parser_test1() {
    let input = "true AND 0";
    let f: Ltl<String> = ltl_parser::FormulaParser::new().parse(input).unwrap();
    print!("parsed {}", f);
    assert_eq!(f, Ltl::And(Box::new(Ltl::True), Box::new(Ltl::False)));
}

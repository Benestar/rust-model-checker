use crate::buechi::NGBA;
use crate::ltl::LtlNNF;
use std::collections::{BTreeSet, HashSet};
use std::fmt::Debug;
use std::hash::Hash;

pub trait LtlToNGBA<AP> {
    type Q: Hash + Eq;
    type S;

    fn to_nba<'a>(&self, ltl: &'a LtlNNF<AP>) -> NGBA<'a, Self::Q, Self::S>;
}

fn is_consistent<'a, AP: Ord + Clone>(ltl: &'a LtlNNF<AP>, set: &BTreeSet<LtlNNF<AP>>) -> bool {
    for s in ltl.subformulas() {
        match s {
            LtlNNF::True => {
                if !set.contains(s) {
                    return false;
                }
            }
            LtlNNF::False => {
                if set.contains(s) {
                    return false;
                }
            }
            LtlNNF::And(x, y) => {
                if set.contains(s) != (set.contains(x.as_ref()) && set.contains(y.as_ref())) {
                    return false;
                }
            }
            LtlNNF::Or(x, y) => {
                if set.contains(s) != (set.contains(x.as_ref()) || set.contains(y.as_ref())) {
                    return false;
                }
            }
            /*
            LtlNNF::NProp(a) => {
                if set.contains(s) == set.contains(&LtlNNF::Prop(a.clone())) {
                    return false;
                }
            }
            LtlNNF::Prop(a) => {
                if set.contains(s) == set.contains(&LtlNNF::NProp(a.clone())) {
                    return false;
                }
            }
            */
            _ => (),
        }
    }

    true
}

struct ConsistentSet<'a, AP> {
    require: BTreeSet<&'a LtlNNF<AP>>,
    forbid: BTreeSet<&'a LtlNNF<AP>>,
}

struct ConsistentSetBuilder<'a, AP> {
    ltl: &'a LtlNNF<AP>,
    require: BTreeSet<&'a LtlNNF<AP>>,
    forbid: BTreeSet<&'a LtlNNF<AP>>,
}

impl<'a, AP: Hash + Ord + Clone + Debug> ConsistentSetBuilder<'a, AP> {
    fn new(ltl: &'a LtlNNF<AP>) -> Self {
        Self {
            ltl,
            require: BTreeSet::new(),
            forbid: BTreeSet::new(),
        }
    }

    fn require(&mut self, ltl: &'a LtlNNF<AP>) {
        self.require.insert(ltl);
    }

    fn forbid(&mut self, ltl: &'a LtlNNF<AP>) {
        self.forbid.insert(ltl);
    }

    fn generate(&self) -> HashSet<BTreeSet<LtlNNF<AP>>> {
        let mut consistent = HashSet::new();

        let num = self.ltl.subformulas().count() - self.require.len() - self.forbid.len();

        for bits in 0..2u128.pow(num as u32) {
            let mut foo: BTreeSet<LtlNNF<AP>> = self.require.iter().map(|&s| s.clone()).collect();

            for (i, s) in self
                .ltl
                .subformulas()
                .filter(|s| !self.require.contains(s) && !self.forbid.contains(s))
                .enumerate()
            {
                if (bits >> i) % 2 == 1 {
                    foo.insert(s.clone());
                }
            }

            dbg!(&foo);

            if is_consistent(self.ltl, &foo) {
                consistent.insert(foo);
                dbg!("consistent");
            }
        }

        //iterate through require and forbid sets
        //fill them with all possible options

        /*
        let mut stack = Vec::new();

        stack.push((self.require.clone(), self.forbid.clone()));

        for s in self.ltl.subformulas() {
            if !self.require.contains(s) && !self.forbid.contains(s) {
                for i in range(stack.len()) {
                    let (r, f) = stack[i];

                    match s {
                        LtlNNF::And(x, y) => {
                            for t in self.ltl.subformulas() {
                                if r.contains(x) && r.contains(y) {
                                    r.insert(s);
                                }

                                if f.contains(x) || f.contains(y) {
                                    f.insert(s);
                                }

                            }
                        }
                    }
                }
            }
        }

        for s in self.ltl.subformulas() {
            match s {
                LtlNNF::And(x, y) => {
                    if self.require.contains(s) {
                        self.require.insert(x);
                        self.require.insert(y);
                        //call with restriction
                        self.require.remove(x);
                        self.require.remove(y);
                    }

                    if self.forbid.contains(s) {
                        self.forbid.insert(x);
                        //call with restriction and reset
                        self.generate();
                        self.forbid.remove(x);
                        self.forbid.insert(y);
                        //call with restriction and reset
                        self.generate();
                        self.forbid.remove(y);
                    }

                    //call with and call without
                }
            }

            if !self.require.contains(s) && !self.forbid.contains(s) {
                match s {}

                self.require.insert(s);
                //recursion
                //reset
                self.forbid.insert(s);
                //recursion
                //reset
            }
        }
        */

        consistent
    }
}

pub struct SimpleLtlToNGBA;

impl<AP: Hash + Ord + Clone + Debug> LtlToNGBA<AP> for SimpleLtlToNGBA {
    type Q = BTreeSet<LtlNNF<AP>>;
    type S = BTreeSet<AP>;

    fn to_nba<'a>(&self, ltl: &'a LtlNNF<AP>) -> NGBA<'a, Self::Q, Self::S> {
        NGBA {
            initial: self.initial(ltl),
            trans: self.trans(ltl),
            accepting: self.accepting(ltl),
        }
    }
}

impl<'a> SimpleLtlToNGBA {
    fn initial<AP: Hash + Ord + Clone + Debug>(
        &self,
        ltl: &LtlNNF<AP>,
    ) -> HashSet<<Self as LtlToNGBA<AP>>::Q> {
        let mut builder = ConsistentSetBuilder::new(ltl);

        builder.require(ltl);

        builder.generate()
    }

    fn trans<AP: Hash + Ord + Clone + Debug>(
        &self,
        ltl: &'a LtlNNF<AP>,
    ) -> Box<
        'a
            + Fn(
                &<Self as LtlToNGBA<AP>>::Q,
                &<Self as LtlToNGBA<AP>>::S,
            ) -> HashSet<<Self as LtlToNGBA<AP>>::Q>,
    > {
        Box::new(move |p, s| {
            // Get formulas of the form Prop(a).
            let props = p.iter().filter_map(|s| {
                if let LtlNNF::Prop(a) = s {
                    Some(a)
                } else {
                    None
                }
            });

            // Consider only transitiions containing exactly these propositions.
            if s.iter().ne(props) {
                dbg!(&s);
                return HashSet::new();
            }

            let mut builder = ConsistentSetBuilder::new(ltl);

            for s in ltl.subformulas() {
                if let LtlNNF::Next(x) = s {
                    if p.contains(s) {
                        builder.require(x);
                    } else {
                        builder.forbid(x);
                    }
                }

                if let LtlNNF::Until(x, y) = s {
                    if p.contains(s) {
                        if !p.contains(x) && !p.contains(y) {
                            return HashSet::new();
                        }

                        if p.contains(x) && !p.contains(y) {
                            builder.require(s);
                        }
                    } else {
                        if p.contains(y) {
                            return HashSet::new();
                        }

                        if p.contains(x) && !p.contains(y) {
                            builder.forbid(s);
                        }
                    }
                }

                if let LtlNNF::Release(x, y) = s {
                    if p.contains(s) {
                        if !p.contains(y) {
                            return HashSet::new();
                        }

                        if !p.contains(x) && p.contains(y) {
                            builder.require(s);
                        }
                    } else {
                        if p.contains(x) && p.contains(y) {
                            return HashSet::new();
                        }

                        if !p.contains(x) && p.contains(y) {
                            builder.forbid(s);
                        }
                    }
                }
            }

            dbg!(&builder.require);
            dbg!(&builder.forbid);

            builder.generate()
        })
    }

    fn accepting<AP: Hash + Ord + Clone + Debug>(
        &self,
        ltl: &'a LtlNNF<AP>,
    ) -> Vec<Box<'a + Fn(&<Self as LtlToNGBA<AP>>::Q) -> bool>> {
        ltl.subformulas()
            .filter_map(|s| {
                if let LtlNNF::Until(x, y) = s {
                    let pred: Box<'a + Fn(&<Self as LtlToNGBA<AP>>::Q) -> bool> =
                        Box::new(move |q| q.contains(y) || !q.contains(s));

                    Some(pred)
                } else {
                    None
                }
            })
            .collect()
    }
}

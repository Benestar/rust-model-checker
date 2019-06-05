use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::fmt::Debug;

pub struct NGBA<'a, Q: Hash + Eq, S> {
    pub initial: HashSet<Q>,
    pub trans: Box<'a + Fn(&Q, &S) -> HashSet<Q>>,
    pub accepting: Vec<Box<'a + Fn(&Q) -> bool>>,
}

#[derive(Debug)]
pub struct ExplicitNGBA<Q: Hash + Eq, S: Hash + Eq> {
    pub initial: HashSet<Q>,
    pub trans: HashSet<(Q, S, Q)>,
    pub accepting: Vec<HashSet<Q>>,
}

struct IndexPool<T> {
    pool: HashMap<T, usize>,
}

impl<T> IndexPool<T>
where
    T: Eq + Hash,
{
    pub fn new() -> Self {
        Self {
            pool: HashMap::new(),
        }
    }

    pub fn contains(&self, t: &T) -> bool {
        self.pool.contains_key(t)
    }

    pub fn get_or_insert(&mut self, t: T) -> usize {
        let len = self.pool.len();

        *self.pool.entry(t).or_insert(len)
    }

    pub fn get(&self, t: &T) -> Option<usize> {
        self.pool.get(t).cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&T, &usize)> {
        self.pool.iter()
    }
}

impl<'a, Q: Hash + Eq, S: Hash + Eq> NGBA<'a, Q, S>
where
    Q: Eq + Hash + Clone + Debug,
    S: Eq + Hash + Clone + Debug,
{
    pub fn explore(&self, alphabet: &Vec<S>) -> ExplicitNGBA<usize, S> {
        let mut initial = HashSet::new();
        let mut trans = HashSet::new();
        let mut accepting = Vec::new();

        let mut pool = IndexPool::new();
        let mut stack = Vec::new();

        for q in self.initial.iter() {
            let qi = pool.get_or_insert(q.clone());
            initial.insert(qi);

            stack.push(q.clone());
        }

        while let Some(p) = stack.pop() {
            // We know that p is registered in the pool.
            let pi = pool.get(&p).unwrap();

            for s in alphabet {
                for q in (self.trans)(&p, s) {
                    dbg!((&p, &q));

                    if !pool.contains(&q) {
                        stack.push(q.clone());
                    }

                    let qi = pool.get_or_insert(q);

                    trans.insert((pi, s.clone(), qi));
                }
            }
        }

        for acc in self.accepting.iter() {
            let mut set = HashSet::new();

            for (q, qi) in pool.iter() {
                if acc(q) {
                    set.insert(*qi);
                }
            }

            accepting.push(set);
        }

        ExplicitNGBA {
            initial,
            trans,
            accepting,
        }
    }
}

use std::collections::{HashMap, HashSet};
use std::hash::Hash;

pub struct NBA<Q, S> {
    initial: HashSet<Q>,
    trans: Box<dyn Fn(&Q, &S) -> HashSet<Q>>,
    accepting: Box<dyn Fn(&Q) -> bool>,
}

pub struct ExplicitNBA<Q, S> {
    initial: HashSet<Q>,
    trans: HashSet<(Q, S, Q)>,
    accepting: HashSet<Q>,
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

impl<Q, S> NBA<Q, S>
where
    Q: Eq + Hash + Clone,
    S: Eq + Hash + Clone,
{
    pub fn explore(&self, alphabet: &HashSet<S>) -> ExplicitNBA<usize, S> {
        let mut initial = HashSet::new();
        let mut trans = HashSet::new();
        let mut accepting = HashSet::new();

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
                    if !pool.contains(&q) {
                        stack.push(q.clone());
                    }

                    let qi = pool.get_or_insert(q);

                    trans.insert((pi, s.clone(), qi));
                }
            }
        }

        for (q, qi) in pool.iter() {
            if (self.accepting)(q) {
                accepting.insert(*qi);
            }
        }

        ExplicitNBA {
            initial,
            trans,
            accepting,
        }
    }
}

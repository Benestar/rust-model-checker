use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;

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

struct Foo<'a, T> {
    pool: HashMap<&'a T, usize>,
    vec: Vec<&'a T>,
}

impl<'a, T> Foo<'a, T>
where
    T: Eq + Hash,
{
    pub fn get_or_insert(&mut self, t: &'a T) -> usize {
        if self.pool.contains_key(t) {
            *self.pool.get(t).unwrap()
        } else {
            self.vec.push(t);
            self.pool.insert(t, self.pool.len());
            self.pool.len() - 1
        }
    }
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

impl<T> IntoIterator for IndexPool<T> {
    type Item = (T, usize);
    type IntoIter = <HashMap<T, usize> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.pool.into_iter()
    }
}

impl<'a, Q: Hash + Eq, S: Hash + Eq> NGBA<'a, Q, S>
where
    Q: Eq + Hash + Clone + Debug,
    S: Eq + Hash + Clone + Debug,
{
    pub fn explore(&self, alphabet: &Vec<S>) -> (Vec<Q>, ExplicitNGBA<usize, S>) {
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

        let mut states: Vec<_> = pool.into_iter().collect();

        states.sort_unstable_by_key(|(_, i)| *i);

        let states: Vec<_> = states.into_iter().map(|(q, _)| q).collect();

        let ngba = ExplicitNGBA {
            initial,
            trans,
            accepting,
        };

        (states, ngba)
    }
}

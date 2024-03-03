use std::rc::Rc;
use crate::do_tests;

pub trait Environment<TKey: PartialEq, TVal> : Clone {
    fn empty() -> Self;

    fn extend(&self, key: TKey, value: TVal) -> Self;

    fn extend_many<const COUNT: usize>(&self, bindings: [(TKey, TVal); COUNT]) -> Self;

    fn try_get(&self, key: TKey) -> Option<&TVal>;
}

pub trait EnvironmentExt<TKey: PartialEq, TVal> : Environment<TKey, TVal> {
    fn try_apply<TOut>(&self, key: TKey, action: impl FnOnce(&TVal) -> TOut) -> Option<TOut>;
}

impl<TKey: PartialEq, TVal, T: Environment<TKey, TVal>> EnvironmentExt<TKey, TVal> for T {
    fn try_apply<TOut>(&self, key: TKey, action: impl FnOnce(&TVal) -> TOut) -> Option<TOut> {
        self.try_get(key).map(action)
    }
}

#[derive(Clone)]
pub enum PairEnvData<TKey: PartialEq, TVal> {
    Empty,
    Extend{ binding: (TKey, TVal), rest: PairEnv<TKey, TVal> }
}

pub struct PairEnv<TKey: PartialEq, TVal>(Rc<PairEnvData<TKey, TVal>>);
impl<TKey: PartialEq, TVal> Clone for PairEnv<TKey, TVal> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<TKey: PartialEq, TVal> Environment<TKey, TVal> for PairEnv<TKey, TVal> {
    fn empty() -> Self { Self(Rc::new(PairEnvData::Empty)) }

    fn extend(&self, key: TKey, value: TVal) -> Self {
        Self(Rc::new(PairEnvData::Extend { binding: (key, value), rest: self.clone() }))
    }

    fn extend_many<const COUNT: usize>(&self, bindings: [(TKey, TVal); COUNT]) -> Self {
        bindings
            .into_iter()
            .rev()
            .fold(self.clone(), |acc, next| acc.extend(next.0, next.1))
    }

    fn try_get(&self, key: TKey) -> Option<&TVal> {
        match self.0.as_ref() {
            PairEnvData::Empty => None,
            PairEnvData::Extend { binding, rest } => {
                if binding.0 == key {
                    Some(&binding.1)
                } else {
                    rest.try_get(key)
                }
            }
        }
    }
}

fn test_it<TEnv: Environment<&'static str, i32>>() {
    let env = TEnv::empty().extend_many([
        ("a", 10),
        ("b", 20),
        ("c", 30)
    ]);

    env.try_apply("a", |v| println!("binding for a in env is {v}"));
    env.try_apply("b", |v| println!("binding for b in env is {v}"));
    env.try_apply("c", |v| println!("binding for c in env is {v}"));

    println!("binding for a exists? {}", env.try_get("a").is_some());
    println!("binding for b exists? {}", env.try_get("b").is_some());
    println!("binding for c exists? {}", env.try_get("c").is_some());
    println!("binding for d exists? {}", env.try_get("d").is_some());

    let env2 = env.extend_many([
        ("b", 100),
        ("a", 1000)
    ]);

    env2.try_apply("a", |v| println!("binding for a in env is {v}"));
    env2.try_apply("b", |v| println!("binding for b in env is {v}"));
    env2.try_apply("c", |v| println!("binding for c in env is {v}"));
}

pub fn ex_2_5() {
    do_tests!(
        test_it;
        "pair repr"<PairEnv<&'static str, i32>>
    );
}
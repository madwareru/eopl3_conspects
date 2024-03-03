use crate::do_tests;

pub trait NatNumber : Clone {
    fn zero() -> Self;

    fn is_zero(&self) -> bool;

    fn succ(&self) -> Self;

    fn pred(&self) -> Option<Self>;
}

pub trait NatNumberExtra : NatNumber {
    fn to_int(&self) -> i32;

    fn plus(&self, rhs: &Self) -> Self;

    fn minus(&self, rhs: &Self) -> Option<Self>;

    fn times(&self, rhs: &Self) -> Self;
}

impl<T> NatNumberExtra for T where T: NatNumber {
    fn to_int(&self) -> i32 {
        self
            .pred()
            .map(|it| 1 + it.to_int())
            .unwrap_or(0)
    }

    fn plus(&self, rhs: &Self) -> Self {
        rhs.pred()
            .map(|pr| self.succ().plus(&pr))
            .unwrap_or_else(|| self.clone())
    }

    fn minus(&self, rhs: &Self) -> Option<Self> {
        match (rhs.pred(), self.pred()) {
            (_, None) => None,
            (None, _) => Some(self.clone()),
            (Some(r), Some(l)) => l.minus(&r)
        }
    }

    fn times(&self, rhs: &Self) -> Self {
        match rhs.pred() {
            Some(r) if !self.is_zero() => self.plus(&self.times(&r)),
            _ => Self::zero()
        }
    }
}

#[derive(Clone)]
pub struct BigInt { bytes: Vec<u8> }

#[inline]
fn inc_with_carry(b: u8) -> (u8, bool) {
    if b < u8::MAX { (b + 1u8, false) } else { (0u8, true) }
}

#[inline]
fn dec_with_carry(b: u8) -> (u8, bool) {
    if b > 0u8 { (b - 1u8, false) } else { (u8::MAX, true) }
}

impl NatNumber for BigInt {
    fn zero() -> Self { Self { bytes: vec![0u8] } }

    fn is_zero(&self) -> bool { self.bytes.iter().all(|it| *it == 0u8) }

    fn succ(&self) -> Self {
        let mut copy_bytes = self.bytes.clone();

        for i in 0..copy_bytes.len() {
            let (b, carry) = inc_with_carry(copy_bytes[i]);
            copy_bytes[i] = b;

            if !carry { break; }

            if i == copy_bytes.len() - 1 {
                copy_bytes.push(1u8);
            }
        }

        Self { bytes: copy_bytes }
    }

    fn pred(&self) -> Option<Self> {
        if self.is_zero() {
            None
        } else {
            let mut copy_bytes = self.bytes.clone();

            for i in 0..copy_bytes.len() {
                let (b, carry) = dec_with_carry(copy_bytes[i]);
                copy_bytes[i] = b;

                if !carry { break; }
            }

            Some(Self { bytes: copy_bytes })
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum UnaryRepr {
    Zero,
    Succ(Box<UnaryRepr>)
}

impl NatNumber for UnaryRepr {
    fn zero() -> Self { Self::Zero }

    fn is_zero(&self) -> bool { *self == Self::Zero }

    fn succ(&self) -> Self { Self::Succ(Box::new(self.clone())) }

    fn pred(&self) -> Option<Self> {
        match self {
            UnaryRepr::Zero => None,
            UnaryRepr::Succ(tail) => Some(*tail.clone())
        }
    }
}

impl NatNumber for i32 {
    fn zero() -> Self { 0 }

    fn is_zero(&self) -> bool { *self == 0 }

    fn succ(&self) -> Self { self + 1 }

    fn pred(&self) -> Option<Self> {
        match self {
            0 => None,
            x => Some(x - 1)
        }
    }
}

fn factorial<TNatNumber: NatNumber>(it: &TNatNumber) -> TNatNumber {
    match it.pred() {
        Some(pr) if !pr.is_zero() => it.times(&factorial(&pr)),
        _ => it.clone(),
    }
}

fn test_it<TNatNumber: NatNumber>() {
    let x = TNatNumber::zero(); println!("{} is zero: {}", x.to_int(), x.is_zero());
    let y = x.succ(); println!("{} is zero: {}", y.to_int(), y.is_zero());
    let z = y.pred().unwrap(); println!("{} is zero: {}", z.to_int(), z.is_zero());
    let w = (y.plus(&y)).plus(&y.plus(&y)); println!("{} is zero: {}", w.to_int(), w.is_zero());
    let a = w.minus(&y).unwrap(); println!("{} is zero: {}", a.to_int(), a.is_zero());
    let b = w.minus(&a).unwrap(); println!("{} is zero: {}", b.to_int(), b.is_zero());
    let c = w.times(&a); println!("{} is zero: {}", c.to_int(), c.is_zero());
    let f0 = factorial(&w); println!("fact({}) = {}", w.to_int(), f0.to_int());
    let six = c.pred().unwrap()
        .pred().unwrap()
        .pred().unwrap()
        .pred().unwrap()
        .pred().unwrap()
        .pred().unwrap();
    let f = factorial(&six); println!("fact({}) = {}", six.to_int(), f.to_int());
}

pub fn ex_2_1() {
    do_tests!(
        test_it;
        "unary repr"<UnaryRepr>,
        "int repr"<i32>,
        "big int repr"<BigInt>
    );
}
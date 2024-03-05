use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use std::rc::Rc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LetListData {
    Empty,
    Cons { head: Box<LetValue>, tail: LetList }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LetList(Rc<LetListData>);

impl Into<LetList> for Vec<LetValue> {
    fn into(self) -> LetList {
        self.into_iter().rev().fold(
            LetList::empty(),
            |acc, next| acc.cons(next)
        )
    }
}

impl IntoLetValue for Vec<LetValue> {
    fn into_let_value(self) -> LetValue {
        let l: LetList = self.into();
        l.into_let_value()
    }
}

impl LetList {
    pub fn empty() -> Self {
        Self(Rc::new(LetListData::Empty))
    }

    pub fn cons(&self, value: LetValue) -> Self {
        Self(Rc::new(LetListData::Cons { head: Box::new(value), tail: self.clone() }))
    }

    pub fn revert(&self) -> Self {
        self.fold(Self::empty(), |acc, next| acc.cons(next.clone()))
    }

    pub fn concat(&self, other: &Self) -> Self {
        self.revert().fold(other.clone(), |acc, next| acc.cons(next.clone()))
    }

    pub fn length(&self) -> i32 {
        self.fold(0, |acc, _| acc + 1)
    }

    pub fn is_empty(&self) -> bool {
        match self.0.as_ref() {
            LetListData::Empty => true,
            LetListData::Cons { .. } => false
        }
    }

    pub fn structural_eq<TVar>(&self, other: &Self) -> Result<LetValue, LetError<TVar>> {
        match (self.0.as_ref(), other.0.as_ref()) {
            (LetListData::Empty, LetListData::Empty) => Ok(true.into_let_value()),
            (
                LetListData::Cons { head: hl, tail: tl },
                LetListData::Cons { head: hr, tail: tr }
            ) => {
                match hl.structural_eq(hr)? {
                    LetValue::Boolean(false) => Ok(false.into_let_value()),
                    LetValue::Boolean(true) => tl.structural_eq(tr),
                    _ => unreachable!()
                }
            },
            _ => Err(LetError::CantCompareTypes {
                l: self.clone().into_let_value(),
                r: other.clone().into_let_value()
            })
        }
    }

    pub fn fold<T>(&self, init: T, reducer: impl Fn(T, &LetValue) -> T) -> T {
        match self.0.as_ref() {
            LetListData::Empty => init,
            LetListData::Cons { head, tail } => {
                let v = reducer(init, head);
                tail.fold(v, reducer)
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LetValue {
    Number(i32),
    Boolean(bool),
    List(LetList)
}

pub trait IntoLetValue {
    fn into_let_value(self) -> LetValue;
}

impl IntoLetValue for i32 {
    fn into_let_value(self) -> LetValue {
        LetValue::Number(self)
    }
}

impl IntoLetValue for bool {
    fn into_let_value(self) -> LetValue {
        LetValue::Boolean(self)
    }
}

impl IntoLetValue for LetList {
    fn into_let_value(self) -> LetValue {
        LetValue::List(self)
    }
}

impl IntoLetValue for LetValue {
    fn into_let_value(self) -> LetValue {
        self
    }
}

impl Display for LetValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LetValue::Number(n) => f.write_fmt(format_args!("{n}")),
            LetValue::Boolean(b) => f.write_fmt(format_args!("{}", if *b { "#t" } else { "#f" })),
            LetValue::List(l) => {
                let nested = l.fold(
                    Vec::new(),
                    |acc, next| {
                        let mut acc = acc;
                        acc.push(format!("{next}"));
                        acc
                    }
                ).join(", ");
                f.write_fmt(format_args!("[{nested}]"))
            }
        }
    }
}

impl LetValue {
    pub fn match_type(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(_), Self::Number(_)) => true,
            (Self::Boolean(_), Self::Boolean(_)) => true,
            (Self::List(_), Self::List(_)) => true,
            _ => false
        }
    }

    pub fn structural_eq<TVar>(&self, other: &Self) -> Result<LetValue, LetError<TVar>> {
        match (self, other) {
            (Self::Number(l), Self::Number(r)) => Ok((*l == *r).into_let_value()),
            (Self::Boolean(l), Self::Boolean(r)) => Ok((*l && *r).into_let_value()),
            (Self::List(l), Self::List(r)) => l.structural_eq(&r),
            _ => Err(LetError::CantCompareTypes { l: self.clone(), r: other.clone() })
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UnOp {
    Car,
    Cdr,
    Empty,
    Length,
    Minus,
    IsZero,
    AsBool,
    Not,
    AsNumber
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BinOp {
    Eq,
    And,
    Or,
    Xor,
    Subtract,
    Cons,
    Concat,
    Add,
    Greater,
    Less,
    GreaterEq,
    LessEq,
    Multiply,
    Division,
    Reminder
}

#[derive(Clone, Debug)]
pub enum LetError<TVar> {
    VarNotFound { bound_name: TVar },
    CantCastToNumber,
    CantCastToBool,
    CantCastToList,
    CantCarFromEmptyList,
    CantCdrFromEmptyList,
    CantCompareTypes { l: LetValue, r: LetValue },
    UnsupportedArithmeticOperator { op: BinOp },
    UnsupportedListOperator { op: BinOp },
    UnsupportedBooleanOperator { op: BinOp },
    UnsupportedComparisonOperator { op: BinOp },
    UnsupportedOperator { op: BinOp, l: LetValue, r: LetValue },
    NoSuitableCondMatchFound,
    FailedToUnpackList
}

pub trait Env<TVar> : Clone {
    fn make() -> Self;

    fn append(&self, bound_name: impl Into<TVar>, value: impl IntoLetValue) -> Self;

    fn get<TRef: ?Sized>(&self, bound_name: &TRef) -> Option<&LetValue>
        where TVar: PartialEq<TRef>;
}

#[derive(Clone)]
pub enum LetEnvData<TVar>{
    Empty,
    Pair{ bound_name: TVar, value: LetValue, tail: LetEnv<TVar> }
}

pub struct LetEnv<TVar>(Rc<LetEnvData<TVar>>);
impl<TVar> Clone for LetEnv<TVar> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> Env<T> for LetEnv<T> {
    fn make() -> Self {
        Self(Rc::new(LetEnvData::Empty))
    }

    fn append(&self, bound_name: impl Into<T>, value: impl IntoLetValue) -> Self {
        let tail = Self(self.0.clone());
        Self(
            Rc::new(
                LetEnvData::Pair {
                    bound_name: bound_name.into(),
                    value: value.into_let_value(),
                    tail
                }
            )
        )
    }

    fn get<TRef: ?Sized>(&self, bound_name: &TRef) -> Option<&LetValue> where T: PartialEq<TRef> {
        match self.0.as_ref() {
            LetEnvData::Empty => None,
            LetEnvData::Pair { bound_name: b_name, value, tail } => {
                if b_name.eq(bound_name) {
                    Some(value)
                } else {
                    tail.get(bound_name)
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum LetExpression<TRef: ?Sized, TVar: PartialEq<TRef> + AsRef<TRef>> {
    EmptyList(PhantomData<TRef>),
    Constant(Box<LetValue>),
    If{
        condition: Box<LetExpression<TRef, TVar>>,
        then: Box<LetExpression<TRef, TVar>>,
        otherwise: Box<LetExpression<TRef, TVar>>
    },
    Cond{
        match_list: Vec<(LetExpression<TRef, TVar>, LetExpression<TRef, TVar>)>,
        else_match: Box<LetExpression<TRef, TVar>>
    },
    Print(Box<LetExpression<TRef, TVar>>),
    Binary {
        op: BinOp,
        l: Box<LetExpression<TRef, TVar>>,
        r: Box<LetExpression<TRef, TVar>>,
    },
    Unary {
        op: UnOp,
        operand:  Box<LetExpression<TRef, TVar>>
    },
    Var(TVar),
    Let {
        bindings: Vec<(TVar, LetExpression<TRef, TVar>)>,
        expr: Box<LetExpression<TRef, TVar>>
    },
    Unpack {
        bindings: Vec<TVar>,
        unpacked_expr: Box<LetExpression<TRef, TVar>>,
        expr: Box<LetExpression<TRef, TVar>>
    }
}

impl<TRef: ?Sized, TVar: PartialEq<TRef> + AsRef<TRef> + Clone> LetExpression<TRef, TVar> {
    pub fn eval(&self, env: &impl Env<TVar>) -> Result<LetValue, LetError<TVar>> {
        match self {
            LetExpression::EmptyList(_) => Ok(LetList::empty().into_let_value()),
            LetExpression::Constant(value) => Ok(value.as_ref().clone()),
            LetExpression::If { condition, then, otherwise } => {
                match condition.eval(env)? {
                    LetValue::Boolean(true) => then.eval(env),
                    LetValue::Boolean(false) => otherwise.eval(env),
                    _ => Err(LetError::CantCastToBool)
                }
            },
            LetExpression::Cond { match_list, else_match } => {
                for match_entry in match_list.iter() {
                    match match_entry.0.eval(env)? {
                        LetValue::Boolean(true) => return match_entry.1.eval(env),
                        LetValue::Boolean(false) => {},
                        _ => return Err(LetError::CantCastToBool)
                    }
                }
                else_match.eval(env)
            }
            LetExpression::Print(expr) => {
                let res = expr.eval(env)?;
                println!("{res}");
                Ok(res)
            },
            LetExpression::Binary { op, l, r } =>{
                let (l_res, r_res) = (l.eval(env)?, r.eval(env)?);
                match (&l_res, &r_res) {
                    (_, _) if *op == BinOp::Eq => l_res.structural_eq(&r_res),
                    (LetValue::Number(ln), LetValue::Number(rn)) => {
                        match op {
                            BinOp::And => Err(LetError::CantCastToBool),
                            BinOp::Or => Err(LetError::CantCastToBool),
                            BinOp::Xor => Err(LetError::CantCastToBool),
                            BinOp::Subtract => Ok((ln - rn).into_let_value()),
                            BinOp::Cons => Err(LetError::CantCastToList),
                            BinOp::Concat => Err(LetError::CantCastToList),
                            BinOp::Add => Ok((ln + rn).into_let_value()),
                            BinOp::Greater => Ok((ln > rn).into_let_value()),
                            BinOp::Less => Ok((ln < rn).into_let_value()),
                            BinOp::GreaterEq => Ok((ln >= rn).into_let_value()),
                            BinOp::LessEq => Ok((ln <= rn).into_let_value()),
                            BinOp::Multiply => Ok((ln * rn).into_let_value()),
                            BinOp::Division => Ok((ln / rn).into_let_value()),
                            BinOp::Reminder => Ok((ln % rn).into_let_value()),
                            _ => unreachable!()
                        }
                    },
                    (LetValue::Boolean(lb), LetValue::Boolean(rb)) => {
                        match op {
                            BinOp::And => Ok((*lb && *rb).into_let_value()),
                            BinOp::Or => Ok((*lb || *rb).into_let_value()),
                            BinOp::Xor => Ok((*lb ^ *rb).into_let_value()),
                            BinOp::Subtract => Err(LetError::CantCastToNumber),
                            BinOp::Cons => Err(LetError::CantCastToList),
                            BinOp::Concat => Err(LetError::CantCastToList),
                            BinOp::Add => Err(LetError::CantCastToNumber),
                            BinOp::Greater => Err(LetError::CantCastToNumber),
                            BinOp::Less => Err(LetError::CantCastToNumber),
                            BinOp::GreaterEq => Err(LetError::CantCastToNumber),
                            BinOp::LessEq => Err(LetError::CantCastToNumber),
                            BinOp::Multiply => Err(LetError::CantCastToNumber),
                            BinOp::Division => Err(LetError::CantCastToNumber),
                            BinOp::Reminder => Err(LetError::CantCastToNumber),
                            _ => unreachable!()
                        }
                    },
                    (l, LetValue::List(rl)) => {
                        match op {
                            BinOp::And => Err(LetError::CantCastToBool),
                            BinOp::Or => Err(LetError::CantCastToBool),
                            BinOp::Xor => Err(LetError::CantCastToBool),
                            BinOp::Subtract => Err(LetError::CantCastToNumber),
                            BinOp::Cons => Ok(rl.cons(l.clone()).into_let_value()),
                            BinOp::Concat => {
                                if let LetValue::List(ll) = l {
                                    Ok(ll.concat(&rl).into_let_value())
                                } else {
                                    Err(LetError::CantCarFromEmptyList)
                                }
                            }
                            BinOp::Add => Err(LetError::CantCastToNumber),
                            BinOp::Greater => Err(LetError::CantCastToNumber),
                            BinOp::Less => Err(LetError::CantCastToNumber),
                            BinOp::GreaterEq => Err(LetError::CantCastToNumber),
                            BinOp::LessEq => Err(LetError::CantCastToNumber),
                            BinOp::Multiply => Err(LetError::CantCastToNumber),
                            BinOp::Division => Err(LetError::CantCastToNumber),
                            BinOp::Reminder => Err(LetError::CantCastToNumber),
                            _ => unreachable!()
                        }
                    },
                    _ => {
                        match op {
                            BinOp::And |
                            BinOp::Or |
                            BinOp::Xor |
                            BinOp::Subtract |
                            BinOp::Cons |
                            BinOp::Concat |
                            BinOp::Add |
                            BinOp::Greater |
                            BinOp::Less |
                            BinOp::GreaterEq |
                            BinOp::LessEq |
                            BinOp::Multiply |
                            BinOp::Division |
                            BinOp::Reminder => Err(
                                LetError::UnsupportedOperator {
                                    op: *op,
                                    l: l_res,
                                    r: r_res
                                }
                            ),
                            _ => unreachable!()
                        }
                    }
                }
            },
            LetExpression::Unary { op, operand } => {
                let res = operand.eval(env)?;
                match res {
                    LetValue::Number(n) => {
                        match op {
                            UnOp::Car |
                            UnOp::Cdr |
                            UnOp::Empty |
                            UnOp::Length => Err(LetError::CantCastToList),
                            UnOp::Minus => Ok((-n).into_let_value()),
                            UnOp::IsZero => Ok((n == 0).into_let_value()),
                            UnOp::AsBool => Ok((n != 0).into_let_value()),
                            UnOp::Not => Err(LetError::CantCastToBool),
                            UnOp::AsNumber => Ok(res.clone())
                        }
                    }
                    LetValue::Boolean(b) => {
                        match op {
                            UnOp::Car |
                            UnOp::Cdr |
                            UnOp::Empty |
                            UnOp::Length => Err(LetError::CantCastToList),
                            UnOp::Minus | UnOp::IsZero => Err(LetError::CantCastToNumber),
                            UnOp::AsBool => Ok(res.clone()),
                            UnOp::Not => Ok((!b).into_let_value()),
                            UnOp::AsNumber => Ok((if b { 1 } else { 0 }).into_let_value())
                        }
                    }
                    LetValue::List(l) => {
                        match op {
                            UnOp::Car => match l.0.as_ref() {
                                LetListData::Empty => Err(LetError::CantCarFromEmptyList),
                                LetListData::Cons { head, .. } => {
                                    Ok(head.clone().into_let_value())
                                }
                            },
                            UnOp::Cdr => match l.0.as_ref() {
                                LetListData::Empty => Err(LetError::CantCdrFromEmptyList),
                                LetListData::Cons { tail, .. } => {
                                    Ok(tail.clone().into_let_value())
                                }
                            },
                            UnOp::Empty => Ok(l.is_empty().into_let_value()),
                            UnOp::Length => Ok(l.length().into_let_value()),
                            UnOp::Minus => Err(LetError::CantCastToNumber),
                            UnOp::IsZero => Err(LetError::CantCastToNumber),
                            UnOp::AsBool => Err(LetError::CantCastToBool),
                            UnOp::Not => Err(LetError::CantCastToBool),
                            UnOp::AsNumber => Err(LetError::CantCastToNumber)
                        }
                    }
                }
            },
            LetExpression::Var(var) => match env.get(var.as_ref()) {
                None => Err(LetError::VarNotFound { bound_name: var.clone() }),
                Some(v) => Ok(v.clone())
            },
            LetExpression::Let { bindings, expr } => {
                let mut new_env = env.clone();
                for binding in bindings.iter() {
                    new_env = new_env.append(binding.0.clone(), binding.1.eval(env)?);
                }
                expr.eval(&new_env)
            },
            LetExpression::Unpack { bindings, unpacked_expr, expr } => {
                let new_env = {
                    let res = unpacked_expr.eval(env)?;
                    match (&bindings[..], res) {
                        (bl, LetValue::List(l)) if bl.len() == l.length() as usize => {
                            let mut new_env = env.clone();
                            let mut l = &l;
                            for i in 0..bl.len() {
                                l = match l.0.as_ref() {
                                    LetListData::Empty => l,
                                    LetListData::Cons { head, tail } => {
                                        new_env = new_env.append(
                                            bl[i].clone(),
                                            head.clone().into_let_value()
                                        );
                                        tail
                                    }
                                };
                            }
                            Ok(new_env)
                        },
                        _ => Err(LetError::FailedToUnpackList)
                    }
                }?;
                expr.eval(&new_env)
            },
        }
    }
}

pub type StrLetExpr = LetExpression<str, String>;

pub fn ex_3_let() {
    let test_cases = [
        StrLetExpr::Constant(Box::new(10.into_let_value())),
        StrLetExpr::Var("a".to_string()),
        StrLetExpr::Unary {
            op: UnOp::IsZero,
            operand: Box::new(StrLetExpr::Var("a".to_string()))
        },
        StrLetExpr::Unary {
            op: UnOp::AsBool,
            operand: Box::new(StrLetExpr::Var("a".to_string()))
        },
        StrLetExpr::Unary {
            op: UnOp::Minus,
            operand: Box::new(StrLetExpr::Var("a".to_string()))
        },
        StrLetExpr::Var("b".to_string()),
        StrLetExpr::Unary {
            op: UnOp::IsZero,
            operand: Box::new(StrLetExpr::Var("b".to_string()))
        },
        StrLetExpr::Unary {
            op: UnOp::AsBool,
            operand: Box::new(StrLetExpr::Var("b".to_string()))
        },
        StrLetExpr::Unary {
            op: UnOp::Minus,
            operand: Box::new(StrLetExpr::Var("b".to_string()))
        },
        StrLetExpr::Var("c".to_string()),
        StrLetExpr::Unary {
            op: UnOp::IsZero,
            operand: Box::new(StrLetExpr::Var("c".to_string()))
        },
        StrLetExpr::Unary {
            op: UnOp::AsBool,
            operand: Box::new(StrLetExpr::Var("c".to_string()))
        },
        StrLetExpr::Unary {
            op: UnOp::Minus,
            operand: Box::new(StrLetExpr::Var("c".to_string()))
        },
        StrLetExpr::Var("d".to_string()),
        StrLetExpr::Unary {
            op: UnOp::IsZero,
            operand: Box::new(StrLetExpr::Var("d".to_string()))
        },
        StrLetExpr::Unary {
            op: UnOp::AsBool,
            operand: Box::new(StrLetExpr::Var("d".to_string()))
        },
        StrLetExpr::Unary {
            op: UnOp::Minus,
            operand: Box::new(StrLetExpr::Var("d".to_string()))
        },
        StrLetExpr::Var("e".to_string()),
        StrLetExpr::Unary {
            op: UnOp::IsZero,
            operand: Box::new(StrLetExpr::Var("e".to_string()))
        },
        StrLetExpr::Unary {
            op: UnOp::AsNumber,
            operand: Box::new(StrLetExpr::Var("e".to_string()))
        },
        StrLetExpr::If {
            condition: Box::new(StrLetExpr::Var("e".to_string())),
            then: Box::new(StrLetExpr::Constant(Box::new(1.into_let_value()))),
            otherwise: Box::new(StrLetExpr::Constant(Box::new(0.into_let_value())))
        },
        StrLetExpr::Binary {
            op: BinOp::Subtract,
            l: Box::new(StrLetExpr::Var("a".to_string())),
            r: Box::new(StrLetExpr::Var("b".to_string())),
        },
        StrLetExpr::Binary {
            op: BinOp::Add,
            l: Box::new(StrLetExpr::Var("a".to_string())),
            r: Box::new(StrLetExpr::Var("b".to_string())),
        },
        StrLetExpr::Binary {
            op: BinOp::Multiply,
            l: Box::new(StrLetExpr::Var("a".to_string())),
            r: Box::new(StrLetExpr::Var("b".to_string())),
        },
        StrLetExpr::Binary {
            op: BinOp::Division,
            l: Box::new(StrLetExpr::Var("a".to_string())),
            r: Box::new(StrLetExpr::Var("b".to_string())),
        },
        StrLetExpr::Binary {
            op: BinOp::Reminder,
            l: Box::new(StrLetExpr::Var("a".to_string())),
            r: Box::new(StrLetExpr::Var("b".to_string())),
        },
        StrLetExpr::Let {
            bindings: vec![
                (
                    "e".to_string(),
                    StrLetExpr::If {
                        condition: Box::new(StrLetExpr::Var("c".to_string())),
                        then: Box::new(StrLetExpr::Constant(Box::new(5.into_let_value()))),
                        otherwise: Box::new(StrLetExpr::Constant(Box::new(10.into_let_value()))),
                    }
                )
            ],
            expr: Box::new(StrLetExpr::Binary {
                op: BinOp::Subtract,
                l: Box::new(
                    StrLetExpr::Binary {
                        op: BinOp::Add,
                        l: Box::new(StrLetExpr::Var("a".to_string())),
                        r: Box::new(StrLetExpr::Var("e".to_string())),
                    },
                ),
                r: Box::new(StrLetExpr::Var("b".to_string())),
            })
        },
        StrLetExpr::Unary {
            op: UnOp::IsZero,
            operand: Box::new(
                StrLetExpr::Let {
                    bindings: vec![
                        ("z".into(), StrLetExpr::Constant(Box::new(2.into_let_value()))),
                        ("x".into(), StrLetExpr::Constant(Box::new(3.into_let_value()))),
                    ],
                    expr: Box::new(
                        StrLetExpr::Let {
                            bindings: vec![
                                ("y".into(), StrLetExpr::Binary{
                                    op: BinOp::Subtract,
                                    l: Box::new(StrLetExpr::Var("x".into())),
                                    r: Box::new(StrLetExpr::Constant(Box::new(1.into_let_value())))
                                }),
                            ],
                            expr: Box::new(
                                StrLetExpr::Let {
                                    bindings: vec![
                                        (
                                            "x".into(),
                                            StrLetExpr::Constant(Box::new(4.into_let_value()))
                                        ),
                                    ],
                                    expr: Box::new(
                                        StrLetExpr::Binary {
                                            op: BinOp::Subtract,
                                            l: Box::new(StrLetExpr::Var("z".into())),
                                            r: Box::new(
                                                StrLetExpr::Binary {
                                                    op: BinOp::Subtract,
                                                    l: Box::new(StrLetExpr::Var("x".into())),
                                                    r: Box::new(StrLetExpr::Var("y".into()))
                                                }
                                            )
                                        }
                                    )
                                }
                            )
                        }
                    )
                }
            )
        },
        StrLetExpr::Let {
            bindings: vec![
                ("x".into(), StrLetExpr::Constant(Box::new(2.into_let_value()))),
                ("y".into(), StrLetExpr::Constant(Box::new(3.into_let_value()))),
            ],
            expr: Box::new(
                StrLetExpr::Let {
                    bindings: vec![
                        ("x".into(), StrLetExpr::Binary{
                            op: BinOp::Subtract,
                            l: Box::new(StrLetExpr::Var("y".into())),
                            r: Box::new(StrLetExpr::Var("x".into()))
                        }),
                    ],
                    expr: Box::new(
                        StrLetExpr::Binary{
                            op: BinOp::Subtract,
                            l: Box::new(StrLetExpr::Var("y".into())),
                            r: Box::new(StrLetExpr::Var("x".into()))
                        }
                    )
                }
            )
        },
        StrLetExpr::Let {
            bindings: vec![
                (
                    "l1".into(),
                    StrLetExpr::Binary {
                        op: BinOp::Cons,
                        l: Box::new(
                            StrLetExpr::Constant(Box::new(1.into_let_value()))
                        ),
                        r: Box::new(
                            StrLetExpr::Constant(Box::new(
                                vec![
                                    2.into_let_value(),
                                    3.into_let_value()
                                ].into_let_value()
                            ))
                        )
                    }
                ),
                (
                    "l2".into(),
                    StrLetExpr::Binary {
                        op: BinOp::Cons,
                        l: Box::new(
                            StrLetExpr::Constant(Box::new(5.into_let_value()))
                        ),
                        r: Box::new(
                            StrLetExpr::Binary {
                                op: BinOp::Cons,
                                l: Box::new(
                                    StrLetExpr::Constant(Box::new(6.into_let_value()))
                                ),
                                r: Box::new(
                                    StrLetExpr::Constant(Box::new(
                                        LetList::empty().into_let_value()
                                    ))
                                )
                            }
                        )
                    }
                ),
            ],
            expr: Box::new(
                StrLetExpr::Binary {
                    op: BinOp::Add,
                    l: Box::new(
                        StrLetExpr::Unary {
                            op: UnOp::Car,
                            operand: Box::new(
                                StrLetExpr::Unary {
                                    op: UnOp::Cdr,
                                    operand: Box::new(StrLetExpr::Var("l1".into()))
                                }
                            )
                        }
                    ),
                    r: Box::new(
                        StrLetExpr::Unary {
                            op: UnOp::Car,
                            operand: Box::new(StrLetExpr::Var("l2".into()))
                        }
                    )
                }
            )
        },
        StrLetExpr::Let {
            bindings: vec![
                (
                    "l1".into(),
                    StrLetExpr::Constant(Box::new(
                        vec![
                            1.into_let_value(),
                            2.into_let_value(),
                            3.into_let_value()
                        ].into_let_value()
                    ))
                ),
                (
                    "l2".into(),
                    StrLetExpr::Constant(Box::new(
                        vec![
                            5.into_let_value(),
                            6.into_let_value()
                        ].into_let_value()
                    ))
                ),
            ],
            expr: Box::new(
                StrLetExpr::Binary {
                    op: BinOp::Add,
                    l: Box::new(
                        StrLetExpr::Unary {
                            op: UnOp::Car,
                            operand: Box::new(
                                StrLetExpr::Unary {
                                    op: UnOp::Cdr,
                                    operand: Box::new(StrLetExpr::Unary {
                                        op: UnOp::Cdr,
                                        operand: Box::new(StrLetExpr::Var("l1".into()))
                                    })
                                }
                            )
                        }
                    ),
                    r: Box::new(
                        StrLetExpr::Unary {
                            op: UnOp::Car,
                            operand: Box::new(StrLetExpr::Var("l2".into()))
                        }
                    )
                }
            )
        },
        StrLetExpr::Unary {
            op: UnOp::IsZero,
            operand: Box::new(
                StrLetExpr::Unpack {
                    bindings: vec!["z".into(), "x".into()],
                    unpacked_expr: Box::new(
                        StrLetExpr::Constant(Box::new(
                            vec![
                                2.into_let_value(),
                                3.into_let_value()
                            ].into_let_value()
                        ))
                    ),
                    expr: Box::new(
                        StrLetExpr::Let {
                            bindings: vec![
                                (
                                    "y".into(),
                                    StrLetExpr::Binary {
                                        op: BinOp::Subtract,
                                        l: Box::new(StrLetExpr::Var("x".into())),
                                        r: Box::new(
                                            StrLetExpr::Constant(Box::new(1.into_let_value()))
                                        )
                                    }
                                )
                            ],
                            expr: Box::new(StrLetExpr::Let {
                                bindings: vec![
                                    (
                                        "x".into(),
                                        StrLetExpr::Constant(Box::new(4.into_let_value()))
                                    )
                                ],
                                expr: Box::new(
                                    StrLetExpr::Binary {
                                        op: BinOp::Subtract,
                                        l: Box::new(StrLetExpr::Var("z".into())),
                                        r: Box::new(
                                            StrLetExpr::Binary {
                                                op: BinOp::Subtract,
                                                l: Box::new(StrLetExpr::Var("x".into())),
                                                r: Box::new(StrLetExpr::Var("y".into())),
                                            }
                                        )
                                    }
                                )
                            })
                        }
                    )
                }
            )
        }
    ];

    let env: LetEnv<String> = LetEnv::make()
        .append("a", 100)
        .append("b", 21)
        .append("c", true)
        .append("d", 0)
        .append("e", false);

    for expr in test_cases {
        println!("{:?}", &expr);
        let result = expr.eval(&env);
        println!("{:?}", result);
        println!();
    }
}
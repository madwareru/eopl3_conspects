use std::error::Error;
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use std::rc::Rc;
use thiserror::Error;

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
    Unused(PhantomData<TRef>),
    Constant(Box<LetValue>), // v
    If { // v
        condition: Box<LetExpression<TRef, TVar>>,
        then: Box<LetExpression<TRef, TVar>>,
        otherwise: Box<LetExpression<TRef, TVar>>
    },
    Cond { // v
        match_list: Vec<(LetExpression<TRef, TVar>, LetExpression<TRef, TVar>)>,
        else_match: Box<LetExpression<TRef, TVar>>
    },
    Print(Box<LetExpression<TRef, TVar>>), // v
    Binary {
        op: BinOp,
        l: Box<LetExpression<TRef, TVar>>,
        r: Box<LetExpression<TRef, TVar>>,
    },
    Unary {
        op: UnOp,
        operand:  Box<LetExpression<TRef, TVar>>
    },
    Var(TVar), // v
    Let {
        bindings: Vec<(TVar, LetExpression<TRef, TVar>)>,
        expr: Box<LetExpression<TRef, TVar>>
    },
    Unpack { // v
        bindings: Vec<TVar>,
        unpacked_expr: Box<LetExpression<TRef, TVar>>,
        expr: Box<LetExpression<TRef, TVar>>
    },
    MakeList (Vec<LetExpression<TRef, TVar>>) // v
}

pub trait Parser<TIn, TOut>
{
    type ErrorType: Error;
    fn parse(input: &TIn) -> Result<TOut, Self::ErrorType>;
    fn un_parse(input: &TOut) -> TIn;
}

#[derive(Clone, Debug)]
pub enum SExpr {
    Id(String),
    Number(i32),
    Bool(bool),
    List(Vec<SExpr>)
}

impl SExpr {
    fn id(data: String) -> Self { Self::Id(data) }

    fn number(n: i32) -> Self { Self::Number(n) }

    fn boolean(b: bool) -> Self { Self::Bool(b) }

    fn list(elements: impl IntoIterator<Item=Self>) -> Self {
        Self::List(elements.into_iter().collect())
    }
}

impl Display for SExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SExpr::Id(id) => f.write_fmt(format_args!("{id}")),
            SExpr::Number(n) => f.write_fmt(format_args!("{n}")),
            SExpr::Bool(b) => f.write_fmt(format_args!("{b}")),
            SExpr::List(l) => {
                let nested = l.iter().map(|it| format!("{it}")).collect::<Vec<_>>().join(" ");
                f.write_fmt(format_args!("( {nested} )"))
            }
        }
    }
}

macro_rules! s_expr(
    (false) => { SExpr::boolean( false ) };
    (true) => { SExpr::boolean( true ) };
    ($id: ident) => { SExpr::id( stringify!($id).to_string() ) };
    ($n: literal) => { SExpr::number( $n ) };
    (($($child_content:tt)+)) => { SExpr::list([$(s_expr![$child_content]),+]) }
);

impl<TRef: ?Sized, TVar: PartialEq<TRef> + AsRef<TRef> + Clone> LetExpression<TRef, TVar> {
    pub fn eval(&self, env: &impl Env<TVar>) -> Result<LetValue, LetError<TVar>> {
        match self {
            LetExpression::Unused(_) => unreachable!(),
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
            LetExpression::MakeList(exprs) => {
                let mut values = Vec::new();
                for expr in exprs {
                    let r = expr.eval(env)?;
                    values.push(r);
                }
                Ok(values.into_let_value())
            }
        }
    }
}

pub type StrLetExpr = LetExpression<str, String>;

pub trait IntoListExpr {
    fn into_list_expr(self) -> StrLetExpr;
}

impl IntoListExpr for Vec<StrLetExpr> {
    fn into_list_expr(self) -> StrLetExpr {
        StrLetExpr::MakeList(self)
    }
}

pub struct StrLetParser;

#[derive(Error, Debug)]
pub enum StrLetParseError {
    #[error("Unknown form `{0}`")]
    UnknownForm(String),
    #[error("Unknown unary operator `{0}`")]
    UnknownUnaryOperator(String),
    #[error("Unknown binary operator `{0}`")]
    UnknownBinaryOperator(String),
    #[error("Non empty match list expected")]
    NonEmptyMatchListExpected,
    #[error("Non empty binding list expected")]
    NonEmptyBindingListExpected,
    #[error("Fail")]
    Fail
}

impl Parser<SExpr, StrLetExpr> for StrLetParser {
    type ErrorType = StrLetParseError;

    fn parse(input: &SExpr) -> Result<StrLetExpr, Self::ErrorType> {
        match input {
            SExpr::Id(id) => Ok(StrLetExpr::Var(id.into())),
            SExpr::Number(n) => Ok(StrLetExpr::Constant(Box::new(n.into_let_value()))),
            SExpr::Bool(b) => Ok(StrLetExpr::Constant(Box::new(b.into_let_value()))),
            SExpr::List(l) => {
                let l = &l[..];
                match l {
                    [ SExpr::Id(if_id), cond, SExpr::Id(then_id), th, SExpr::Id(else_id), ls]
                    if if_id.eq("if") && then_id.eq("then") && else_id.eq("else") => {
                        Ok(StrLetExpr::If {
                            condition: Box::new(Self::parse(cond)?),
                            then: Box::new(Self::parse(th)?),
                            otherwise: Box::new(Self::parse(ls)?)
                        })
                    },
                    [ SExpr::Id(make_list_id), ..] if make_list_id.eq("make_list") => {
                        let rest = &l[1..];
                        let mut expressions = Vec::new();
                        for e in rest.iter() {
                            expressions.push(Self::parse(e)?);
                        }
                        Ok(StrLetExpr::MakeList(expressions))
                    },
                    [
                        SExpr::Id(let_id),
                        ..,
                        SExpr::Id(in_id),
                        expr
                    ] if let_id.eq("let") && in_id.eq("in") => {
                        let expr = Box::new(Self::parse(expr)?);
                        let binds = &l[1..l.len()-2];
                        if binds.is_empty() || binds.len() % 3 != 0 {
                            Err(StrLetParseError::NonEmptyBindingListExpected)
                        } else {
                            let mut bindings = Vec::new();
                            for i in 0..binds.len() / 3 {
                                let offset = i * 3;
                                let next_binding = match &binds[offset..offset+3] {
                                    [SExpr::Id(binding), SExpr::Id(id), v] if id.eq("as") => {
                                        Ok((binding.to_string(), Self::parse(v)?))
                                    },
                                    _ => Err(StrLetParseError::Fail)
                                }?;
                                bindings.push(next_binding);
                            }
                            Ok(StrLetExpr::Let { bindings, expr })
                        }
                    },
                    [
                        SExpr::Id(cond_id),
                        ..,
                        SExpr::Id(else_id),
                        ls
                    ] if cond_id.eq("cond") && else_id.eq("else") => {
                        let else_match = Box::new(Self::parse(ls)?);
                        let matches = &l[1..l.len()-2];
                        if matches.is_empty() || matches.len() % 4 != 0 {
                            Err(StrLetParseError::NonEmptyMatchListExpected)
                        } else {
                            let mut match_list = Vec::new();
                            for i in 0..matches.len() / 4 {
                                let offset = i * 4;
                                let next_match = match &matches[offset..offset+4] {
                                    [SExpr::Id(when_id), wh, SExpr::Id(then_id), th]
                                    if when_id.eq("when") && then_id.eq("then") => {
                                        Ok((Self::parse(wh)?, Self::parse(th)?))
                                    },
                                    _ => Err(StrLetParseError::Fail)
                                }?;
                                match_list.push(next_match);
                            }
                            Ok(StrLetExpr::Cond { match_list, else_match })
                        }
                    },
                    [ SExpr::Id(print_id), expr] if print_id.eq("print") => {
                        Ok(StrLetExpr::Print(Box::new(Self::parse(expr)?)))
                    },
                    [
                        SExpr::Id(unpack_id),
                        id_list,
                        SExpr::Id(from_id),
                        unpacked_expr,
                        SExpr::Id(in_id),
                        expr
                    ] if unpack_id.eq("unpack") && from_id.eq("from") && in_id.eq("in") => {
                        let unpacked_expr = Box::new(Self::parse(unpacked_expr)?);
                        let expr = Box::new(Self::parse(expr)?);
                        match id_list {
                            SExpr::List(ids) => {
                                if ids.is_empty() {
                                    Err(StrLetParseError::Fail)
                                } else {
                                    let mut bindings = Vec::new();
                                    for id in ids.iter() {
                                        let next_binding = if let SExpr::Id(id) = id {
                                            Ok(id.to_string())
                                        } else {
                                            Err(StrLetParseError::Fail)
                                        }?;
                                        bindings.push(next_binding);
                                    }
                                    Ok(StrLetExpr::Unpack { bindings, unpacked_expr, expr })
                                }
                            },
                            _ => Err(StrLetParseError::Fail)
                        }
                    },
                    [ SExpr::Id(bin_op), op_l, op_r ] => {
                        let op = match bin_op.as_ref() {
                            "add" => BinOp::Add,
                            "sub" => BinOp::Subtract,
                            "mul" => BinOp::Multiply,
                            "div" => BinOp::Division,
                            "rem" => BinOp::Reminder,
                            "eq" => BinOp::Eq,
                            "and" => BinOp::And,
                            "or" => BinOp::Or,
                            "xor" => BinOp::Xor,
                            "cons" => BinOp::Cons,
                            "concat" => BinOp::Concat,
                            "gt" => BinOp::Greater,
                            "lt" => BinOp::Less,
                            "gt_eq" => BinOp::GreaterEq,
                            "lt_eq" => BinOp::LessEq,
                            _ => return Err(
                                StrLetParseError::UnknownBinaryOperator(bin_op.to_string())
                            )
                        };
                        Ok(
                            StrLetExpr::Binary {
                                op,
                                l: Box::new(Self::parse(op_l)?),
                                r: Box::new(Self::parse(op_r)?)
                            }
                        )
                    },
                    [ SExpr::Id(un_op), operand ] => {
                        let operand = Box::new(Self::parse(operand)?);
                        let op = match un_op.as_ref() {
                            "car" => UnOp::Car,
                            "cdr" => UnOp::Cdr,
                            "is_empty" => UnOp::Empty,
                            "length" => UnOp::Length,
                            "minus" => UnOp::Minus,
                            "is_zero" => UnOp::IsZero,
                            "as_bool" => UnOp::AsBool,
                            "not" => UnOp::Not,
                            "as_number" => UnOp::AsNumber,
                            _ => return Err(
                                StrLetParseError::UnknownUnaryOperator(un_op.to_string())
                            )
                        };
                        Ok(StrLetExpr::Unary { op, operand })
                    },
                    _ => Err(StrLetParseError::UnknownForm(format!("{input}")))
                }
            }
        }
    }

    fn un_parse(_input: &StrLetExpr) -> SExpr {
        todo!()
    }
}

pub fn ex_3_let() {
    let test_cases = [
        s_expr!( 10 ),
        s_expr!( false ),
        s_expr!( ( make_list 1 2 3 ) ),
        s_expr!( a ),
        s_expr!( ( is_zero a ) ),
        s_expr!( ( as_bool a ) ),
        s_expr!( ( minus a ) ),
        s_expr!( b ),
        s_expr!( ( is_zero b ) ),
        s_expr!( ( as_bool b ) ),
        s_expr!( ( minus b ) ),
        s_expr!( c ),
        s_expr!( ( is_zero c ) ),
        s_expr!( ( as_bool c ) ),
        s_expr!( ( minus c ) ),
        s_expr!( d ),
        s_expr!( ( is_zero d ) ),
        s_expr!( ( as_bool d ) ),
        s_expr!( ( minus d ) ),
        s_expr!( e ),
        s_expr!( ( is_zero e ) ),
        s_expr!( ( as_number e ) ),
        s_expr!( ( if e then 1 else 0 ) ),
        s_expr!( ( sub a b ) ),
        s_expr!( ( add a b ) ),
        s_expr!( ( mul a b ) ),
        s_expr!( ( div a b ) ),
        s_expr!( ( rem a b ) ),
        s_expr!( ( let e as ( if c then 5 else 10 ) in ( sub ( add a e ) b ) ) ),
        s_expr!( ( let e as true in ( if e then 100500 else false ) ) ),
        s_expr!(
            ( is_zero
                ( let z as 2 x as 3 in
                    ( let y as ( sub x 1 ) in
                        ( let x as 4 in ( sub z ( sub x y ) ) ) ) ) ) ),
        s_expr!(
            ( let x as 2 y as 3 in
                ( let x as ( sub y x ) in
                    ( sub y x ) ) ) ),
        s_expr!(
            ( let
                l1 as ( cons 1 ( make_list 2 3 ) )
                l2 as ( cons 5 ( cons 6 ( make_list ) ) )
                in
                ( add
                    ( car ( cdr l1 ) )
                    ( car l2 ) ) ) ),
        s_expr!(
            ( let
                l1 as ( make_list 1 2 3 )
                l2 as ( make_list 5 6 )
                in
                ( add
                    ( car ( cdr ( cdr l1 ) ) )
                    ( car l2 ) ) ) ),
        s_expr!(
            ( is_zero
                ( unpack ( z x ) from ( make_list 2 3 ) in
                    ( let y as ( sub x 1 ) in
                        ( let x as 4 in ( sub z ( sub x y ) ) ) ) ) ) ),
        s_expr!(
            ( unpack ( l1 l2 )
                from ( make_list
                    ( print ( make_list 1 2 3 ) )
                    ( print ( make_list 4 5 ) ) ) in
                ( let l as ( print ( concat l1 l2 ) ) in
                    ( print ( make_list
                        ( eq ( length l ) 3 )
                        ( eq ( length l1 ) 3 )
                        ( eq ( length l2 ) 3 )
                        ( eq l ( make_list 1 2 3 4 5 ) ) ) ) ) ) ),
        s_expr!(
            ( let l as ( make_list 1 true ( make_list 1 2 3 ) ( make_list false false false ) ) in
                ( print l ) ) ),
        s_expr!(
            ( let
                x as 4
                y as 3
                z as true
                in
                ( cond
                    when ( eq x 5 ) then x
                    when ( eq y 3 ) then y
                    when ( not z ) then z
                    else 42 ) ) ),
        s_expr!(
            ( let
                x as 3
                y as 5
                z as true
                in
                ( cond
                    when ( eq x 5 ) then x
                    when ( eq y 3 ) then y
                    when ( not z ) then z
                    else 42 ) ) )
    ];

    let env: LetEnv<String> = LetEnv::make()
        .append("a", 100)
        .append("b", 21)
        .append("c", true)
        .append("d", 0)
        .append("e", false);

    for s_expr in test_cases.iter() {
        println!("{s_expr}");
        match StrLetParser::parse(s_expr) {
            Ok(expr) => {
                println!("{:?}", &expr);
                let result = expr.eval(&env);
                println!("{:?}", result);
            }
            Err(error) => {
                println!("Failed to parse! {error}")
            }
        }
        println!();
    }
}
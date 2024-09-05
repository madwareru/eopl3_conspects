use crate::{
    chapter_3::shared_s_expr::{Parser, SExpr},
    s_expr_ch3,
};
use std::{
    fmt::{Debug, Formatter},
    rc::Rc,
};
use thiserror::Error;

pub trait Evaluatable {
    fn eval(&self, env: &LetProcEnv) -> Result<LetProcValue, LetProcError>;
}

#[derive(Clone, Debug)]
pub enum LetProcValue {
    Number(i32),
    Boolean(bool),
    UserProc(LetProcUser),
    IntrinsicProc(LetProcIntrinsic),
}

impl LetProcValue {
    pub fn as_bool(&self) -> Result<bool, LetProcError> {
        match self {
            LetProcValue::Boolean(b) => Ok(*b),
            _ => Err(LetProcError::CantCastToBool),
        }
    }

    pub fn as_num(&self) -> Result<i32, LetProcError> {
        match self {
            LetProcValue::Number(n) => Ok(*n),
            _ => Err(LetProcError::CantCastToNumber),
        }
    }

    pub fn call(&self, bindings: &[LetProcValue]) -> Result<LetProcValue, LetProcError> {
        match self {
            LetProcValue::UserProc(user_proc) => {
                match (user_proc.arguments.len(), bindings.len()) {
                    (a, b) if a == b => {
                        let new_env = user_proc
                            .arguments
                            .iter()
                            .zip(bindings.iter())
                            .fold(user_proc.env.clone(), |new_env, (name, v)| {
                                new_env.append(name, v.clone())
                            });

                        let new_env_ensured = match &user_proc.recursive_name {
                            Some(rec_name) => new_env.append(rec_name, self.clone()),
                            _ => new_env,
                        };

                        user_proc.body.eval(&new_env_ensured)
                    }
                    (a, b) if a < b => {
                        let partial_bindings = &bindings[0..a];
                        let rest_bindings = &bindings[a..];

                        let new_env = user_proc
                            .arguments
                            .iter()
                            .zip(partial_bindings.iter())
                            .fold(user_proc.env.clone(), |new_env, (name, v)| {
                                new_env.append(name, v.clone())
                            });

                        let new_env_ensured = match &user_proc.recursive_name {
                            Some(rec_name) => new_env.append(rec_name, self.clone()),
                            _ => new_env,
                        };

                        let res = user_proc.body.eval(&new_env_ensured)?;

                        let safe_arguments = rest_bindings
                            .iter()
                            .map(|it| LetProcExpression::Value(it.clone()))
                            .map(|it| it.eval(&new_env_ensured))
                            .collect::<Result<Vec<_>, LetProcError>>()?;

                        res.call(&safe_arguments)
                    }
                    (a, b) if a > b => {
                        let partial_args = &user_proc.arguments[0..b];
                        let rest_args = &user_proc.arguments[b..];

                        let new_env = partial_args
                            .iter()
                            .zip(bindings.iter())
                            .fold(user_proc.env.clone(), |new_env, (name, v)| {
                                new_env.append(name, v.clone())
                            });

                        let new_env_ensured = match &user_proc.recursive_name {
                            Some(rec_name) => new_env.append(rec_name, self.clone()),
                            _ => new_env,
                        };

                        Ok((
                            new_env_ensured,
                            PartialApplication {
                                bindings: bindings.iter().map(|it| it.clone()).collect(),
                                rest_args: rest_args.iter().map(|it| it.clone()).collect(),
                                wrapped: self.clone(),
                            },
                        )
                            .into_let_proc_value())
                    }
                    _ => unreachable!(),
                }
            }
            LetProcValue::IntrinsicProc(intrinsic_proc) => {
                match (intrinsic_proc.arguments.len(), bindings.len()) {
                    (a, b) if a == b => {
                        let new_env = intrinsic_proc
                            .arguments
                            .iter()
                            .zip(bindings.iter())
                            .fold(intrinsic_proc.env.clone(), |new_env, (name, v)| {
                                new_env.append(name, v.clone())
                            });
                        intrinsic_proc.body.eval(&new_env)
                    }
                    (a, b) if a < b => {
                        let partial_bindings = &bindings[0..a];
                        let rest_bindings = &bindings[a..];

                        let new_env = intrinsic_proc
                            .arguments
                            .iter()
                            .zip(partial_bindings.iter())
                            .fold(intrinsic_proc.env.clone(), |new_env, (name, v)| {
                                new_env.append(name, v.clone())
                            });

                        let res = intrinsic_proc.body.eval(&new_env)?;

                        let safe_arguments = rest_bindings
                            .iter()
                            .map(|it| LetProcExpression::Value(it.clone()))
                            .map(|it| it.eval(&new_env))
                            .collect::<Result<Vec<_>, LetProcError>>()?;

                        res.call(&safe_arguments)
                    }
                    (a, b) if a > b => {
                        let partial_args = &intrinsic_proc.arguments[0..b];
                        let rest_args = &intrinsic_proc.arguments[b..];

                        let new_env = partial_args
                            .iter()
                            .zip(bindings.iter())
                            .fold(intrinsic_proc.env.clone(), |new_env, (name, v)| {
                                new_env.append(name, v.clone())
                            });

                        Ok((
                            new_env,
                            PartialApplication {
                                bindings: bindings.iter().map(|it| it.clone()).collect(),
                                rest_args: rest_args.iter().map(|it| it.clone()).collect(),
                                wrapped: self.clone(),
                            },
                        )
                            .into_let_proc_value())
                    }
                    _ => unreachable!(),
                }
            }
            _ => Err(LetProcError::CantCallNonProcValue),
        }
    }
}

#[derive(Clone, Debug)]
pub struct LetProcUser {
    recursive_name: Option<String>,
    arguments: Vec<String>,
    env: LetProcEnv,
    body: Rc<LetProcExpression>,
}

#[derive(Error, Debug)]
pub enum LetProcError {
    #[error("Can't compare values")]
    CantCompareValues,
    #[error("Can't cast to boolean the value")]
    CantCastToBool,
    #[error("Can't cast to number")]
    CantCastToNumber,
    #[error("Can't call non procedure value")]
    CantCallNonProcValue,
    #[error("Can't bind non procedure value as recursive")]
    CantBindRecursiveToNonProc,
    #[error("Name `{bound_name}` not found")]
    NameNotFound { bound_name: String },
    #[error("Unknown error")]
    Unknown,
}

impl IntoLetProcValue for LetProcValue {
    fn into_let_proc_value(self) -> LetProcValue {
        self
    }
}

impl<TRes: IntoLetProcValue> Evaluatable
    for fn(&LetProcValue, &LetProcValue) -> Result<TRes, LetProcError>
{
    fn eval(&self, env: &LetProcEnv) -> Result<LetProcValue, LetProcError> {
        let lhs = env
            .get("__lhs__")
            .ok_or_else(|| LetProcError::NameNotFound {
                bound_name: "__lhs__".into(),
            })?;

        let rhs = env
            .get("__rhs__")
            .ok_or_else(|| LetProcError::NameNotFound {
                bound_name: "__rhs__".into(),
            })?;

        self(lhs, rhs).map(|it| it.into_let_proc_value())
    }
}

impl<TRes: IntoLetProcValue + 'static> IntoLetProcValue
    for fn(&LetProcValue, &LetProcValue) -> Result<TRes, LetProcError>
{
    fn into_let_proc_value(self) -> LetProcValue {
        LetProcValue::IntrinsicProc(LetProcIntrinsic {
            arguments: vec!["__lhs__".into(), "__rhs__".into()],
            env: LetProcEnv::make(),
            body: Rc::new(self),
        })
    }
}

impl<TRes: IntoLetProcValue> Evaluatable for fn(&LetProcValue) -> Result<TRes, LetProcError> {
    fn eval(&self, env: &LetProcEnv) -> Result<LetProcValue, LetProcError> {
        let operand = env
            .get("__operand__")
            .ok_or_else(|| LetProcError::NameNotFound {
                bound_name: "__operand__".into(),
            })?;

        self(operand).map(|it| it.into_let_proc_value())
    }
}

impl<TRes: IntoLetProcValue + 'static> IntoLetProcValue
    for fn(&LetProcValue) -> Result<TRes, LetProcError>
{
    fn into_let_proc_value(self) -> LetProcValue {
        LetProcValue::IntrinsicProc(LetProcIntrinsic {
            arguments: vec!["__operand__".into()],
            env: LetProcEnv::make(),
            body: Rc::new(self),
        })
    }
}

struct CallSelf;
impl Evaluatable for CallSelf {
    fn eval(&self, env: &LetProcEnv) -> Result<LetProcValue, LetProcError> {
        match env.get("__proc__") {
            Some(proc) => match proc {
                LetProcValue::IntrinsicProc(_) | LetProcValue::UserProc(_) => {
                    let proc_expr = LetProcExpression::Value(proc.clone());
                    LetProcExpression::Call {
                        callee: Rc::new(proc_expr.clone()),
                        args: vec![proc_expr],
                    }
                    .eval(env)
                }
                _ => Err(LetProcError::CantCallNonProcValue),
            },
            _ => Err(LetProcError::NameNotFound {
                bound_name: "__proc__".into(),
            }),
        }
    }
}

impl IntoLetProcValue for CallSelf {
    fn into_let_proc_value(self) -> LetProcValue {
        LetProcValue::IntrinsicProc(LetProcIntrinsic {
            arguments: vec!["__proc__".to_string()],
            env: LetProcEnv::make(),
            body: Rc::new(self),
        })
    }
}

struct PartialApplication {
    bindings: Vec<LetProcValue>,
    rest_args: Vec<String>,
    wrapped: LetProcValue,
}

impl Evaluatable for PartialApplication {
    fn eval(&self, nested_env: &LetProcEnv) -> Result<LetProcValue, LetProcError> {
        let mut args: Vec<LetProcExpression> = self
            .bindings
            .iter()
            .map(|it| LetProcExpression::Value(it.clone()))
            .collect();

        for arg in self.rest_args.iter() {
            let binding = match nested_env.get(arg) {
                None => Err(LetProcError::NameNotFound {
                    bound_name: arg.into(),
                }),
                Some(b) => Ok(b.clone()),
            }?;
            args.push(LetProcExpression::Value(binding));
        }

        let callee = Rc::new(LetProcExpression::Value(self.wrapped.clone()));

        LetProcExpression::Call { callee, args }.eval(nested_env)
    }
}

impl IntoLetProcValue for (LetProcEnv, PartialApplication) {
    fn into_let_proc_value(self) -> LetProcValue {
        LetProcValue::IntrinsicProc(LetProcIntrinsic {
            arguments: self.1.rest_args.clone(),
            env: self.0,
            body: Rc::new(self.1),
        })
    }
}

#[derive(Clone)]
pub struct LetProcIntrinsic {
    arguments: Vec<String>,
    env: LetProcEnv,
    body: Rc<dyn Evaluatable>,
}

impl Debug for LetProcIntrinsic {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "Intrinsic function with args {:?}",
            self.arguments
        ))
    }
}

pub trait IntoLetProcValue {
    fn into_let_proc_value(self) -> LetProcValue;
}

#[derive(Clone, Debug)]
pub enum LetProcEnvData {
    Empty,
    Pair {
        bound_name: String,
        value: LetProcValue,
        tail: LetProcEnv,
    },
}
#[derive(Clone, Debug)]
pub struct LetProcEnv(Rc<LetProcEnvData>);

impl IntoLetProcValue for i32 {
    fn into_let_proc_value(self) -> LetProcValue {
        LetProcValue::Number(self)
    }
}

impl IntoLetProcValue for bool {
    fn into_let_proc_value(self) -> LetProcValue {
        LetProcValue::Boolean(self)
    }
}

impl LetProcEnv {
    pub fn make() -> Self {
        Self(Rc::new(LetProcEnvData::Empty))
    }

    pub fn append(&self, bound_name: impl Into<String>, value: impl IntoLetProcValue) -> Self {
        let tail = Self(self.0.clone());
        Self(Rc::new(LetProcEnvData::Pair {
            bound_name: bound_name.into(),
            value: value.into_let_proc_value(),
            tail,
        }))
    }

    pub fn get(&self, bound_name: &str) -> Option<&LetProcValue> {
        if let LetProcEnvData::Pair {
            bound_name: b_name,
            value,
            tail,
        } = self.0.as_ref()
        {
            if !b_name.eq(bound_name) {
                tail.get(bound_name)
            } else {
                Some(value)
            }
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
pub enum LetProcExpression {
    Value(LetProcValue),
    Name(String),
    Lambda {
        arguments: Vec<String>,
        body: Rc<LetProcExpression>,
    },
    If {
        condition: Rc<LetProcExpression>,
        then: Rc<LetProcExpression>,
        otherwise: Rc<LetProcExpression>,
    },
    Let {
        recursive: bool,
        bound_name: String,
        value: Rc<LetProcExpression>,
        expr: Rc<LetProcExpression>,
    },
    Call {
        callee: Rc<LetProcExpression>,
        args: Vec<LetProcExpression>,
    },
}

impl Evaluatable for LetProcExpression {
    fn eval(&self, env: &LetProcEnv) -> Result<LetProcValue, LetProcError> {
        match self {
            LetProcExpression::Value(v) => Ok(v.clone()),
            LetProcExpression::Name(n) => {
                env.get(n)
                    .map(|it| it.clone())
                    .ok_or_else(|| LetProcError::NameNotFound {
                        bound_name: n.into(),
                    })
            }
            LetProcExpression::Lambda { arguments, body } => {
                Ok(LetProcValue::UserProc(LetProcUser {
                    recursive_name: None,
                    arguments: arguments.clone(),
                    env: env.clone(),
                    body: body.clone(),
                }))
            }
            LetProcExpression::If {
                condition,
                then,
                otherwise,
            } => match condition.eval(env)? {
                LetProcValue::Boolean(true) => then.eval(env),
                LetProcValue::Boolean(false) => otherwise.eval(env),
                _ => Err(LetProcError::CantCastToBool),
            },
            LetProcExpression::Let {
                recursive,
                bound_name,
                value,
                expr,
            } => {
                let v = match (*recursive, value.eval(env)?) {
                    (false, v) => Ok(v),
                    (true, LetProcValue::UserProc(user_proc)) => {
                        Ok(LetProcValue::UserProc(LetProcUser {
                            recursive_name: Some(bound_name.into()),
                            ..user_proc
                        }))
                    }
                    _ => Err(LetProcError::CantBindRecursiveToNonProc),
                }?;

                let new_env = env.append(bound_name, v);

                expr.eval(&new_env)
            }
            LetProcExpression::Call { callee, args } => {
                let safe_arguments = args
                    .iter()
                    .map(|it| it.eval(env))
                    .collect::<Result<Vec<_>, LetProcError>>()?;

                let proc = callee.eval(env)?;

                proc.call(&safe_arguments)
            }
        }
    }
}

pub struct StrLetProcParser;

#[derive(Error, Debug)]
pub enum StrLetProcParseError {
    #[error("Detected non-id node at identifier position")]
    NonIdNodeAtIdentifierPosition,
    #[error("Detected garbage at the end of let form")]
    GarbageAtTheEndOfLetForm,
    #[error("Detected garbage at the end of if form")]
    GarbageAtTheEndOfIfForm,
    #[error("Detected unknown form")]
    UnknownForm,
}

impl Parser<SExpr, LetProcExpression> for StrLetProcParser {
    type ErrorType = StrLetProcParseError;

    fn parse(input: &SExpr) -> Result<LetProcExpression, Self::ErrorType> {
        match input {
            SExpr::Id(id) => Ok(LetProcExpression::Name(id.into())),
            SExpr::Number(n) => Ok(LetProcExpression::Value((*n).into_let_proc_value())),
            SExpr::Bool(b) => Ok(LetProcExpression::Value((*b).into_let_proc_value())),
            SExpr::List(l) => {
                let l = &l[..];
                match l {
                    [SExpr::Id(if_id), cond, SExpr::Id(then_id), th, SExpr::Id(else_id), ls, ..]
                        if if_id.eq("if") && then_id.eq("then") && else_id.eq("else") =>
                    {
                        if l.len() == 6 {
                            Ok(LetProcExpression::If {
                                condition: Rc::new(Self::parse(cond)?),
                                then: Rc::new(Self::parse(th)?),
                                otherwise: Rc::new(Self::parse(ls)?),
                            })
                        } else {
                            Err(StrLetProcParseError::GarbageAtTheEndOfIfForm)
                        }
                    }
                    [SExpr::Id(proc_id), .., body] if proc_id.eq("proc") => {
                        let args = &l[1..l.len() - 1];
                        let mut arguments = Vec::with_capacity(args.len());
                        for arg in args.iter() {
                            let arg = if let SExpr::Id(id) = arg {
                                Ok(id)
                            } else {
                                Err(StrLetProcParseError::NonIdNodeAtIdentifierPosition)
                            }?;
                            arguments.push(arg.into())
                        }
                        Ok(LetProcExpression::Lambda {
                            arguments,
                            body: Rc::new(Self::parse(body)?),
                        })
                    }
                    [SExpr::Id(let_id), SExpr::Id(bound_id), SExpr::Id(as_id), value, SExpr::Id(in_id), expr, ..]
                        if let_id.eq("let") && as_id.eq("as") && in_id.eq("in") =>
                    {
                        if l.len() == 6 {
                            Ok(LetProcExpression::Let {
                                recursive: false,
                                bound_name: bound_id.into(),
                                value: Rc::new(Self::parse(value)?),
                                expr: Rc::new(Self::parse(expr)?),
                            })
                        } else {
                            Err(StrLetProcParseError::GarbageAtTheEndOfLetForm)
                        }
                    }
                    [SExpr::Id(let_id), SExpr::Id(rec_id), SExpr::Id(bound_id), SExpr::Id(as_id), value, SExpr::Id(in_id), expr, ..]
                        if let_id.eq("let")
                            && rec_id.eq("rec")
                            && as_id.eq("as")
                            && in_id.eq("in") =>
                    {
                        if l.len() == 7 {
                            Ok(LetProcExpression::Let {
                                recursive: true,
                                bound_name: bound_id.into(),
                                value: Rc::new(Self::parse(value)?),
                                expr: Rc::new(Self::parse(expr)?),
                            })
                        } else {
                            Err(StrLetProcParseError::GarbageAtTheEndOfLetForm)
                        }
                    }
                    [callee, ..] => {
                        let callee = Rc::new(Self::parse(callee)?);
                        let rest = &l[1..];
                        let mut args = Vec::with_capacity(rest.len());
                        for argument in rest.iter() {
                            args.push(Self::parse(argument)?);
                        }
                        Ok(LetProcExpression::Call { callee, args })
                    }
                    _ => Err(StrLetProcParseError::UnknownForm),
                }
            }
        }
    }

    fn un_parse(_input: &LetProcExpression) -> SExpr {
        unimplemented!()
    }
}

pub fn ex_3_let_proc() {
    let test_cases = [
        s_expr_ch3![ ( print ( or false false ) ) ],
        s_expr_ch3![ ( print ( or false true ) ) ],
        s_expr_ch3![ ( print ( or true false ) ) ],
        s_expr_ch3![ ( print ( or true true ) ) ],
        s_expr_ch3![ ( print ( and false false ) ) ],
        s_expr_ch3![ ( print ( and false true ) ) ],
        s_expr_ch3![ ( print ( and true false ) ) ],
        s_expr_ch3![ ( print ( and true true ) ) ],
        s_expr_ch3![ ( print ( eq 1 0 ) ) ],
        s_expr_ch3![ ( print ( eq 0 0 ) ) ],
        s_expr_ch3![ ( print ( add 3 4 ) ) ],
        s_expr_ch3![ ( print ( sub 3 4 ) ) ],
        s_expr_ch3![ ( print ( mul 3 4 ) ) ],
        s_expr_ch3![ ( print ( div 20 3 ) ) ],
        s_expr_ch3![ ( print ( mul 3 4 ) ) ],
        s_expr_ch3![
            ( let add2 as { x y -> ( add x y ) } in
            ( print ( add2 3 4 ) ) )
        ],
        s_expr_ch3![
            ( let carried_add as { x -> { y -> ( add x y ) } } in
            ( print ( ( carried_add 3 ) 4 ) ) )
        ],
        s_expr_ch3![
            ( let carried_or as ( proc x ( proc y ( or x y ) ) ) in
            ( let carried_and as ( proc x ( proc y ( and x y ) ) ) in
            ( print ( ( carried_and false ) ( ( carried_or true ) false ) ) ) ) )
        ],
        s_expr_ch3![
            ( let print_true as ( proc ( print true ) ) in
            ( print_true ) )
        ],
        s_expr_ch3![
            ( let f as ( proc x ( sub x 11 ) ) in
            ( f ( f 77 ) ) )
        ],
        s_expr_ch3![ ( ( proc f ( f ( f 77 ) ) ) ( proc x ( sub x 11 ) ) ) ],
        s_expr_ch3![
            ( let make_mult as
                ( proc maker
                    ( proc x
                        ( if ( eq 0 x)
                            then 0
                            else ( add ( ( call_self maker ) ( sub x 1 ) ) 4 ) ) ) ) in
            ( let times4 as ( call_self make_mult ) in
            ( times4 3 ) ) )
        ],
        s_expr_ch3![
            ( let one_or_less as ( proc n ( not ( gt n 1 ) ) ) in
            ( let pred as ( proc n ( sub n 1 ) ) in
            ( let factorial as
                ( call_self
                    ( proc maker
                        ( proc n
                            ( if ( one_or_less n )
                                then n
                                else ( mul n ( ( call_self maker ) ( pred n ) ) ) ) ) ) ) in

            ( print ( factorial 9 ) ) ) ) )
        ],
        s_expr_ch3![
            ( let one_or_less as { n -> ( not ( gt n 1 ) ) } in
            ( let pred as { n -> ( sub n 1 ) } in
            ( let rec factorial as
                { n ->
                    ( if ( one_or_less n )
                        then n
                        else ( mul n ( factorial ( pred n ) ) ) )
                } in
            ( print ( factorial 8 ) ) ) ) )
        ],
        s_expr_ch3![ ( let add_five as { r -> ( add 5 r ) } in ( add_five 20 ) ) ],
        s_expr_ch3![ ( let add_five as ( add 5 ) in ( add_five 20 ) ) ],
        s_expr_ch3![ ( let add2 as { l r -> ( add l r ) } in ( add2 5 20 ) ) ],
        s_expr_ch3![ ( let add2 as ( add ) in ( add2 5 20 ) ) ],
        s_expr_ch3![
            ( let carried_add_three as
                { x ->
                    { y ->
                        { z ->
                            ( add ( add x y ) z )
                        }
                    }
                } in
            ( carried_add_three 1 2 3 ) )
        ],
        s_expr_ch3![
            /*
                let rec times = { x y ->
                    if y == 0 then 0
                    else if y < 0 then times(x, -y)
                    else x + times(x, y - 1)
                };
                let time-three = times(3, _);
                time-three 5
            */
            ( let rec times as
                { x y ->
                    ( if ( lt x 0 )
                        then ( times (minus x) )
                        else ( if (eq x 0 )
                            then 0
                            else ( add y ( times (sub x 1) y ) ) ) ) } in
            ( let times_three as ( times 3 ) in
            ( times_three 5 ) ) )
        ],
    ];

    let env = make_env_with_intrinsics();

    for s_expr in test_cases.iter() {
        println!("{s_expr}");
        match StrLetProcParser::parse(s_expr) {
            Ok(expr) => {
                println!("{:?}", &expr);
                let result = expr.eval(&env);
                println!("{:?}", result);
            }
            Err(error) => println!("Failed to parse! {error}"),
        }
        println!();
    }
}

fn make_env_with_intrinsics() -> LetProcEnv {
    let mut env = LetProcEnv::make().append("call_self", CallSelf);

    macro_rules! add_unary_intrinsics (
        ($($id:ident => $e:expr),+) => {
            $({
                env = env.append(
                    stringify!($id),
                    $e as fn(&LetProcValue) -> _
                );
            })+
        }
    );

    macro_rules! add_binary_intrinsics (
        ($($id:ident => $e:expr),+) => {
            $({
                env = env.append(
                    stringify!($id),
                    $e as fn(&LetProcValue, &LetProcValue) -> _
                );
            })+
        };
    );

    add_binary_intrinsics! {
        add => |l, r| Ok(l.as_num()? + r.as_num()?),
        sub => |l, r| Ok(l.as_num()? - r.as_num()?),
        mul => |l, r| Ok(l.as_num()? * r.as_num()?),
        div => |l, r| Ok(l.as_num()? / r.as_num()?),
        lt => |l, r| Ok(l.as_num()? < r.as_num()?),
        gt => |l, r| Ok(l.as_num()? > r.as_num()?),
        lt_eq => |l, r| Ok(l.as_num()? <= r.as_num()?),
        gt_eq => |l, r| Ok(l.as_num()? >= r.as_num()?),
        and => |l, r| Ok(l.as_bool()? && r.as_bool()?),
        or => |l, r| Ok(l.as_bool()? || r.as_bool()?),
        xor => |l, r| Ok(l.as_bool()? ^ r.as_bool()?),
        eq => |l, r| match (l.as_bool(), r.as_bool(), l.as_num(), r.as_num()) {
            (Ok(lb), Ok(rb), _, _) => Ok(lb == rb),
            (_, _, Ok(ln), Ok(rn)) => Ok(ln == rn),
            _ => Err(LetProcError::CantCompareValues)
        }
    }

    add_unary_intrinsics! {
        identity => |op| Ok(op.clone()),
        not => |op| Ok(!op.as_bool()?),
        minus => |op| Ok(-op.as_num()?),
        print => |op| { println!("{:?}", op); Ok(op.clone()) }
    }

    env
}

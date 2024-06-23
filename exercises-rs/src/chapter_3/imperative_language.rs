use crate::declare_arena;
use crate::shared::arena::{HandleId, ReleaseTemps, TempsCount, TypedArena};

declare_arena! {
    ImperativeLangArena(
        0: String, // reusable strings for names
        1: ImperativeEnvData,
        2: LangValueData,
        3: StatementData,
        4: ExpressionData,
        5: FunctionData
    )
}

pub struct LangContext {
    pub evaluation_arena: ImperativeLangArena
}

impl LangContext {
    pub fn new() -> Self {
        Self {
            evaluation_arena: ImperativeLangArena::new()
        }
    }
}

#[derive(Copy, Clone)]
pub enum LangValueData {
    Number(i32),
    Boolean(bool),
    Unit
}
#[derive(Copy, Clone)]
pub struct LangValue(HandleId<LangValueData>);

pub trait IntoLangValue {
    fn into_lang_value(self, lang_context: &mut LangContext) -> LangValue;
}

impl IntoLangValue for i32 {
    fn into_lang_value(self, lang_context: &mut LangContext) -> LangValue {
        LangValue(
            lang_context.evaluation_arena.alloc_temp(
                || LangValueData::Number(self),
                | to_reset | *to_reset = LangValueData::Number(self)
            )
        )
    }
}

impl IntoLangValue for bool {
    fn into_lang_value(self, lang_context: &mut LangContext) -> LangValue {
        LangValue(
            lang_context.evaluation_arena.alloc_temp(
                || LangValueData::Boolean(self),
                | to_reset | *to_reset = LangValueData::Boolean(self)
            )
        )
    }
}

impl IntoLangValue for LangValue {
    fn into_lang_value(self, _lang_context: &mut LangContext) -> LangValue {
        self
    }
}

#[derive(Copy, Clone)]
pub enum ImperativeEnvData{
    Empty,
    Pair{ bound_name: HandleId<String>, value: LangValue, tail: ImperativeEnv }
}
#[derive(Copy, Clone)]
pub struct ImperativeEnv(HandleId<ImperativeEnvData>);

impl ImperativeEnv {
    pub fn empty(lang_context: &mut LangContext) -> Self {
        let handle = lang_context.evaluation_arena.alloc_temp(
            || ImperativeEnvData::Empty,
            |it| *it = ImperativeEnvData::Empty
        );
        Self(handle)
    }

    pub fn append(
        self,
        lang_context: &mut LangContext,
        name: &str,
        value: impl IntoLangValue
    ) -> Self {
        let value = value.into_lang_value(lang_context);
        let bound_name = lang_context.evaluation_arena.alloc_temp(
            || name.to_string(),
            |to_reset| { to_reset.clear(); *to_reset += name; }
        );

        let handle = lang_context.evaluation_arena.alloc_temp(
            || ImperativeEnvData::Pair { bound_name, value, tail: self },
            |to_reset| *to_reset = ImperativeEnvData::Pair {
                bound_name,
                value,
                tail: self
            }
        );
        Self(handle)
    }

    pub fn get(self, lang_context: &mut LangContext, name: &str) -> Option<LangValue> {
        let env = lang_context.evaluation_arena.get(self.0)?;
        let ImperativeEnvData::Pair { bound_name, value, tail } = env else { return None; };
        let bound_name = lang_context.evaluation_arena.get(*bound_name)?;

        if bound_name.eq(name) {
            Some(*value)
        } else {
            tail.get(lang_context, name)
        }
    }

    pub fn set(
        self,
        lang_context: &mut LangContext,
        name: &str,
        value: impl IntoLangValue
    ) -> bool {
        let v = value.into_lang_value(lang_context);
        match (self.get(lang_context, name), lang_context.evaluation_arena.get(v.0)) {
            (Some(handle), Some(&v)) => {
                lang_context.evaluation_arena.get_mut(handle.0).map(|it| {
                    match (it, v) {
                        (LangValueData::Number(n), LangValueData::Number(v)) => { *n = v; true },
                        (LangValueData::Boolean(b), LangValueData::Boolean(v)) => { *b = v; true }
                        _ => false
                    }
                }).unwrap_or(false)
            },
            _ => false
        }
    }
}


#[derive(Clone)]
pub enum FunctionData {
    UserFunction { arg_names: Vec<HandleId<String>>, body: Vec<Statement> }
}
#[derive(Copy, Clone)]
pub struct Function(HandleId<FunctionData>);

#[derive(Clone)]
pub enum StatementData {
    VarDecl { bound_name: HandleId<String>, binding: Expression },
    Assignment { bound_name: HandleId<String>, binding: Expression },
    IfElse{ condition: Expression, then_body: Vec<Statement>, else_body: Option<Vec<Statement>> },
    WhileLoop{ condition: Expression, body: Vec<Statement> },
    Expression(Expression),
    Return(Expression),
    Break,
    Continue,
}
#[derive(Copy, Clone)]
pub struct Statement(HandleId<StatementData>);

#[derive(Clone)]
pub enum ExpressionData {
    Value(LangValue),
    Name(HandleId<String>),
    Call{ function_name: HandleId<String>, args: Vec<Expression> }
}
#[derive(Copy, Clone)]
pub struct Expression(HandleId<ExpressionData>);

pub fn ex_3_imperative_lang() {
    let mut context = LangContext::new();
    let env = ImperativeEnv::empty(&mut context)
        .append(&mut context, "a", 10)
        .append(&mut context, "b", false);

    let temps_count = context.evaluation_arena.temps_count();

    println!("initial temps count is {:?}", temps_count);

    for name in ["a", "b"] {
        if let Some(a_handle) = env.get(&mut context, name) {
            let value = context.evaluation_arena.get(a_handle.0);
            let found = match value {
                None => {
                    println!("no {name} found for handle!");
                    false
                }
                Some(LangValueData::Number(n)) => {
                    println!("found number {name} = {n}");
                    true
                }
                Some(LangValueData::Boolean(b)) => {
                    println!("found boolean {name} = {b}");
                    true
                }
                Some(LangValueData::Unit) => {
                    println!("found unit {name}");
                    true
                }
            };
            if found {
                println!("trying to change {name} to contain 42");
                if !env.set(&mut context, name, 42) {
                    println!("failed to change! Make sure the name is bounded to number");
                }
            }
        } else {
            println!("no {name} found!");
        }
    }

    for name in ["a", "b", "c", "d", "e"] {
        if let Some(a_handle) = env.get(&mut context, name) {
            let value = context.evaluation_arena.get(a_handle.0);
            match value {
                None => {
                    println!("no {name} found for handle!");
                }
                Some(LangValueData::Number(n)) => {
                    println!("found number {name} = {n}");
                }
                Some(LangValueData::Boolean(b)) => {
                    println!("found boolean {name} = {b}");
                }
                Some(LangValueData::Unit) => {
                    println!("found unit {name}")
                }
            };
        } else {
            println!("no {name} found!");
        }
    }

    let temps_count_after = context.evaluation_arena.temps_count();
    println!("temps count after is {:?}", temps_count_after);

    context.evaluation_arena.release_temporaries(temps_count);

    let temps_count_after = context.evaluation_arena.temps_count();
    println!("temps count after release is {:?}", temps_count_after);
}
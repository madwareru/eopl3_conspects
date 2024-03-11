use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub enum SExpr {
    Id(String),
    Number(i32),
    Bool(bool),
    List(Vec<SExpr>)
}

impl SExpr {
    pub fn id(data: impl Into<String>) -> Self { Self::Id(data.into()) }

    pub fn number(n: i32) -> Self { Self::Number(n) }

    pub fn boolean(b: bool) -> Self { Self::Bool(b) }

    pub fn list(elements: impl IntoIterator<Item=Self>) -> Self {
        Self::List(elements.into_iter().collect())
    }
}

pub trait Parser<TIn, TOut>
{
    type ErrorType: Error;
    fn parse(input: &TIn) -> Result<TOut, Self::ErrorType>;
    fn un_parse(input: &TOut) -> TIn;
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

#[macro_export]
macro_rules! s_expr_ch3 (
    (false) => { SExpr::boolean( false ) };
    (true) => { SExpr::boolean( true ) };
    ($id: ident) => { SExpr::id( stringify!($id) ) };
    ($n: literal) => { SExpr::number( $n ) };
    ({ $($v_name:ident)* -> $expr_content:tt }) => {
        SExpr::list([
            SExpr::id("proc")
            $(, s_expr_ch3![$v_name])*
            , s_expr_ch3![$expr_content]
        ])
    };
    ([$($child_content:tt)*]) => {
        SExpr::list([
            SExpr::id("make_list")
            $(, s_expr_ch3![$child_content])*
        ])
    };
    (($($child_content:tt)+)) => {
        SExpr::list([
            $(s_expr_ch3![$child_content]),+
        ])
    }
);
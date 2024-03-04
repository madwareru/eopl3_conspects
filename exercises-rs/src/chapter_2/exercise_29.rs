use std::fmt::{Display, Formatter};

#[derive(Clone, Copy, Debug)]
pub enum LExprParseError {
    ExactPairExpected,
    ListExpected,
    IdentifierExpected,
    RandsExpectedToBeNonEmpty,
    UnexpectedLambdaIdentifierOccured,
    Fail
}

pub trait Parser<TIn, TOut>
{
    type ErrorType;
    fn parse(input: &TIn) -> Result<TOut, Self::ErrorType>;
    fn un_parse(input: &TOut) -> TIn;
}

#[derive(Clone, Debug)]
pub enum SExpression {
    Id(String),
    List(Vec<SExpression>)
}

impl SExpression {
    fn id(data: &str) -> Self { Self::Id(data.to_string()) }

    fn list(elements: impl IntoIterator<Item=Self>) -> Self {
        Self::List(elements.into_iter().collect())
    }
}

impl Display for SExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SExpression::Id(id) => f.write_fmt(format_args!("{}", id)),
            SExpression::List(l) => {
                f.write_fmt(
                    format_args!(
                        "( {} )",
                        l.iter().map(|item| format!("{item}")).collect::<Vec<_>>().join(" ")
                    )
                )
            }
        }
    }
}

macro_rules! s_expr(
    ($id: ident) => { SExpression::id( stringify!($id) ) };
    (($($child_content:tt)+)) => { SExpression::list([$(s_expr![$child_content]),+]) }
);

#[derive(Clone, Debug)]
pub enum LExpression {
    Id(String),
    Application(Box<LExpression>, Vec<LExpression>),
    Lambda(Vec<String>, Box<LExpression>)
}

impl LExpression {
    fn id(data: &str) -> Self { Self::Id(data.to_string()) }

    fn application(rator: Self, rands: impl IntoIterator<Item=Self>) -> Self {
        Self::Application(Box::new(rator), rands.into_iter().collect())
    }

    fn lambda(bound_vars: impl IntoIterator<Item=String>, body: Self) -> Self {
        Self::Lambda(bound_vars.into_iter().collect(), Box::new(body))
    }
}

struct LExpressionParser;

impl Parser<SExpression, LExpression> for LExpressionParser {
    type ErrorType = LExprParseError;

    fn parse(input: &SExpression) -> Result<LExpression, Self::ErrorType> {
        match input {
            SExpression::Id(id) => {
                if id == "lambda" {
                    Err(LExprParseError::UnexpectedLambdaIdentifierOccured)
                } else {
                    Ok(LExpression::id(id))
                }
            },
            SExpression::List(entries) => {
                match &entries[..] {
                    [x, ..] => {
                        match x {
                            SExpression::Id(id) if id == "lambda" => {
                                match &entries[1..] {
                                    [x, y] => {
                                        match x {
                                            SExpression::List(bound_names) => {
                                                let mut names = Vec::new();
                                                for name in bound_names.iter() {
                                                    let id = {
                                                        if let SExpression::Id(id) = name {
                                                            Ok(id)
                                                        } else {
                                                            Err(LExprParseError::IdentifierExpected)
                                                        }
                                                    }?;
                                                    names.push(id.to_string());
                                                }
                                                let body = Self::parse(y)?;
                                                Ok(LExpression::lambda(names, body))
                                            },
                                            _ => Err(LExprParseError::ListExpected)
                                        }
                                    },
                                    _ => {
                                        Err(LExprParseError::ExactPairExpected)
                                    }
                                }
                            }
                            _ => {
                                let rator = Self::parse(x)?;
                                let mut rands = Vec::new();
                                for entry in (&entries[1..]).iter() {
                                    let rand = Self::parse(entry)?;
                                    rands.push(rand);
                                }
                                if rands.is_empty() {
                                    Err(LExprParseError::RandsExpectedToBeNonEmpty)
                                } else {
                                    Ok(LExpression::application(rator, rands))
                                }
                            }
                        }
                    },
                    _ => {
                        Err(LExprParseError::ListExpected)
                    }
                }
            }
        }
    }

    fn un_parse(input: &LExpression) -> SExpression {
        match input {
            LExpression::Id(id) => SExpression::id(id),
            LExpression::Application(rator, rands) => {
                SExpression::list({
                    let mut v = vec![Self::un_parse(rator)];
                    v.extend(rands.iter().map(|it| Self::un_parse(it)));
                    v
                })
            },
            LExpression::Lambda(names, body) => {
                SExpression::list([
                    SExpression::id("lambda"),
                    SExpression::list(names.into_iter().map(|it| SExpression::id(it))),
                    Self::un_parse(body)
                ])
            }
        }
    }
}

pub fn ex_2_29() {
    let sources = [
        s_expr!{ ( lambda ( a b c ) ( add a b c ) ) },
        s_expr!{ ( ( lambda ( a ) ( a b ) ) c ) },
        s_expr!{ ( lambda ( x ) ( lambda ( y ) ( ( lambda ( x ) ( x y ) ) x ) ) ) },
        s_expr!{ ( lambda ( a b c ) ( add a b c ) ) },
    ];

    for source in sources.into_iter() {
        println!("{:?}", &source);
        println!("{}", &source);

        let l_expr = LExpressionParser::parse(&source).unwrap();
        println!("{:?}", &l_expr);
        let un_parsed = LExpressionParser::un_parse(&l_expr);
        println!("{:?}", &un_parsed);
        println!("{}", &un_parsed);

        println!()
    }
}
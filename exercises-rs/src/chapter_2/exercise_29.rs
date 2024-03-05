use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Clone, Copy, Debug)]
pub enum LExprParseError {
    PairOfNamesAndBodyExpected,
    ListExpected,
    IdentifierExpected,
    RandsExpectedToBeNonEmpty,
    UnexpectedLambdaIdentifier,
    Fail
}

impl Display for LExprParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let err = match self {
            LExprParseError::PairOfNamesAndBodyExpected => "PairOfNamesAndBodyExpected",
            LExprParseError::ListExpected => "ListExpected",
            LExprParseError::IdentifierExpected => "IdentifierExpected",
            LExprParseError::RandsExpectedToBeNonEmpty => "RandsExpectedToBeNonEmpty",
            LExprParseError::UnexpectedLambdaIdentifier => "UnexpectedLambdaIdentifier",
            LExprParseError::Fail => "Fail",
        };
        write!(f, "{}", err)
    }
}

impl Error for LExprParseError {}

pub trait Parser<TIn, TOut>
{
    type ErrorType: Error;
    fn parse(input: &TIn) -> Result<TOut, Self::ErrorType>;
    fn un_parse(input: &TOut) -> TIn;
}

#[derive(Clone, Debug)]
pub enum SExpr {
    Id(String),
    List(Vec<SExpr>)
}

impl SExpr {
    fn id(data: String) -> Self { Self::Id(data) }

    fn list(elements: impl IntoIterator<Item=Self>) -> Self {
        Self::List(elements.into_iter().collect())
    }
}

impl Display for SExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SExpr::Id(id) => f.write_fmt(format_args!("{}", id)),
            SExpr::List(l) => {
                let nested = l.iter().map(|it| format!("{it}")).collect::<Vec<_>>().join(" ");
                f.write_fmt(format_args!("( {nested} )"))
            }
        }
    }
}

macro_rules! s_expr(
    ($id: ident) => { SExpr::id( stringify!($id).to_string() ) };
    (($($child_content:tt)+)) => { SExpr::list([$(s_expr![$child_content]),+]) }
);

#[derive(Clone, Debug)]
pub enum LExpr {
    Id(String),
    Application { rator: Box<LExpr>, rands: Vec<LExpr> },
    Lambda { bound_names: Vec<String>, body: Box<LExpr> }
}

impl LExpr {
    fn id(data: String) -> Self { Self::Id(data) }

    fn application(rator: Self, rands: impl IntoIterator<Item=Self>) -> Self {
        Self::Application {
            rator: Box::new(rator),
            rands: rands.into_iter().collect()
        }
    }

    fn lambda(bound_vars: impl IntoIterator<Item=String>, body: Self) -> Self {
        Self::Lambda {
            bound_names: bound_vars.into_iter().collect(),
            body: Box::new(body)
        }
    }
}

struct LExprParser;

impl Parser<SExpr, LExpr> for LExprParser {
    type ErrorType = LExprParseError;

    fn parse(input: &SExpr) -> Result<LExpr, Self::ErrorType> {
        let parse_id = |id: &str| {
            (id != "lambda")
                .then(|| LExpr::id(id.to_string()))
                .ok_or(LExprParseError::UnexpectedLambdaIdentifier)
        };

        let parse_lambda = |entries: &[SExpr]| {
            match entries {
                [SExpr::List(bound_names), body] => {
                    let bound_names = bound_names.iter()
                        .map(|name| match name {
                            SExpr::Id(id) => (id != "lambda")
                                .then(|| id.to_string())
                                .ok_or(LExprParseError::UnexpectedLambdaIdentifier),
                            _ => Err(LExprParseError::IdentifierExpected)
                        }).collect::<Result<Vec<_>, _>>()?;

                    let body = Self::parse(body)?;

                    Ok(LExpr::lambda(bound_names, body))
                },
                _ => Err(LExprParseError::PairOfNamesAndBodyExpected),
            }
        };

        let parse_application = |head: &SExpr, tail: &[SExpr]| {
            Ok(
                LExpr::application(
                    Self::parse(head)?,
                    tail.iter()
                        .map(Self::parse)
                        .collect::<Result<Vec<_>, _>>()
                        .and_then(|it| {
                            if it.is_empty() {
                                Err(LExprParseError::RandsExpectedToBeNonEmpty)
                            } else {
                                Ok(it)
                            }
                        })?
                )
            )
        };

        match input {
            SExpr::Id(id) => parse_id(id),
            SExpr::List(entries) => match &entries[..] {
                [head, ..] => {
                    let tail = &entries[1..];
                    match head {
                        SExpr::Id(id) if id == "lambda" => parse_lambda(tail),
                        _ => parse_application(head, tail)
                    }
                }
                _ => Err(LExprParseError::ListExpected),
            },
        }
    }

    fn un_parse(input: &LExpr) -> SExpr {
        match input {
            LExpr::Id(id) => SExpr::id(id.to_string()),
            LExpr::Application { rator, rands } => {
                SExpr::list({ [Self::un_parse(rator)]
                    .into_iter()
                    .chain(rands.iter().map(Self::un_parse))
                })
            },
            LExpr::Lambda { bound_names, body } => {
                SExpr::list([
                    SExpr::id("lambda".to_string()),
                    SExpr::list(bound_names.into_iter().map(|it| SExpr::id(it.to_string()))),
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
        s_expr!{ ( lambda ( a b c ) ( add ) ) }, // will fail intentionally
        s_expr!{ ( lambda ( a b c ) ( add a b c ) d ) }, // will fail intentionally
        s_expr!{ ( lambda ( a ( b ) c ) ( add a b c ) ) }, // will fail intentionally
    ];

    for source in sources.into_iter() {
        println!("{:?}", &source);
        println!("{}", &source);

        match LExprParser::parse(&source) {
            Ok(l_expr) => {
                println!("{:?}", &l_expr);
                let un_parsed = LExprParser::un_parse(&l_expr);
                println!("{:?}", &un_parsed);
                println!("{}", &un_parsed);
            }
            Err(err) => println!("Failed to parse {source}, reason: {:?}", err)
        }

        println!()
    }
}
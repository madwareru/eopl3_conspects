pub trait LambdaExpr<TVar: PartialEq<Self::RefType>> {
    type RefType: ?Sized;

    fn var(bound_name: &Self::RefType) -> Self;

    fn lambda(bound_name: &Self::RefType, body: Self) -> Self;

    fn apply(rator: Self, rand: Self) -> Self;

    fn cases<TOut>(
        &self,
        case_var: impl FnOnce(&TVar) -> TOut,
        case_lambda: impl FnOnce(&TVar, &Self) -> TOut,
        case_application: impl FnOnce(&Self, &Self) -> TOut
    ) -> TOut;
}

pub trait LambdaExprExt<TVar: PartialEq<Self::RefType>> : LambdaExpr<TVar> {
    fn occurs_free(&self, search_name: &Self::RefType) -> bool;
}

impl<TVar: PartialEq<Self::RefType>, T : LambdaExpr<TVar>> LambdaExprExt<TVar> for T {
    fn occurs_free(&self, search_name: &Self::RefType) -> bool {
        self.cases(
            |name| name.eq(search_name),
            |bound_name, body| !bound_name.eq(search_name) && body.occurs_free(search_name),
            |rator, rand| rator.occurs_free(search_name) || rand.occurs_free(search_name)
        )
    }
}

fn test_it<LE: LambdaExpr<String, RefType = str>>()
{
    let first = LE::apply(
        LE::lambda(
            "a",
            LE::apply(
                LE::var("a"),
                LE::var("b")
            )
        ),
        LE::var("c")
    );

    for c in ["a", "b", "c", "d"] {
        let occurs = LE::occurs_free(&first, c);
        println!("{c} occurs free in first? {occurs}");
    }

    let second = LE::lambda(
        "x",
        LE::lambda(
            "y",
            LE::apply(
                LE::lambda(
                    "x",
                    LE::apply(
                        LE::var("x"),
                        LE::var("y")
                    )
                ),
                LE::var("x")
            )
        )
    );

    for c in ["x", "y", "z"] {
        let occurs = LE::occurs_free(&second, c);
        println!("{c} occurs free in first? {occurs}");
    }
}

enum LcExpressionData {
    Var { bound_name: String },
    Lambda { bound_name: String, body: LcExpression },
    Application { rator: LcExpression, rand: LcExpression }
}

pub struct LcExpression(Box<LcExpressionData>);

impl LambdaExpr<String> for LcExpression {
    type RefType = str;

    fn var(bound_name: &Self::RefType) -> Self {
        Self( Box::new( LcExpressionData::Var { bound_name: bound_name.to_string() } ) )
    }

    fn lambda(bound_name: &Self::RefType, body: Self) -> Self {
        Self( Box::new( LcExpressionData::Lambda {
            bound_name: bound_name.to_string(),
            body
        } ) )
    }

    fn apply(rator: Self, rand: Self) -> Self {
        Self( Box::new( LcExpressionData::Application { rator, rand } ) )
    }

    fn cases<TOut>(
        &self,
        case_var: impl FnOnce(&String) -> TOut,
        case_lambda: impl FnOnce(&String, &Self) -> TOut,
        case_application: impl FnOnce(&Self, &Self) -> TOut
    ) -> TOut {
        match self.0.as_ref() {
            LcExpressionData::Var { bound_name } =>
                case_var(bound_name),

            LcExpressionData::Lambda { bound_name, body } =>
                case_lambda(bound_name, body),

            LcExpressionData::Application { rator, rand } =>
                case_application(rator, rand)
        }
    }
}

pub fn ex_2_15()
{
    test_it::<LcExpression>()
}
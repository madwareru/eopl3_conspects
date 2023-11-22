package exercises

data class LambdaExpr<TLcExp, TVar>(val boundVar: TVar, val body: TLcExp)
data class ApplicationExpr<TLcExp>(val rator: TLcExp, val rand: TLcExp)

data class LcExpScope<TLcExp, TVar> (
    // constructors:
    val varExpr: (TVar) -> TLcExp,
    val lambdaExpr: (TVar, TLcExp) -> TLcExp,
    val appExpr: (TLcExp, TLcExp) -> TLcExp,

    // extractors (predicates aren't needed):
    val asVarExpr: (TLcExp) -> Option<TVar>,
    val asLambdaExpr: (TLcExp) -> Option<LambdaExpr<TLcExp, TVar>>,
    val asAppExpr: (TLcExp) -> Option<ApplicationExpr<TLcExp>>
) {
    private inline fun <TRes> match(
        exp: TLcExp,
        caseVar: (TVar) -> TRes,
        caseLambda: (LambdaExpr<TLcExp, TVar>) -> TRes,
        caseApplication: (ApplicationExpr<TLcExp>) -> TRes
    ): TRes =
        asVarExpr(exp).map(caseVar).unwrapOr {
            asLambdaExpr(exp).map(caseLambda).unwrapOr {
                asAppExpr(exp).map(caseApplication).unwrapOr {
                    throw IllegalArgumentException("exp is neither var, lambda or app")
                }
            }
        }

    fun occursFree(searchVar: TVar, exp: TLcExp): Boolean =
        match(
            exp,
            caseVar = { it == searchVar },
            caseLambda = { it.boundVar != searchVar && occursFree(searchVar, it.body)},
            caseApplication = { occursFree(searchVar, it.rator) || occursFree(searchVar, it.rand) }
        )
}

sealed class LcExpression {
    data class Var(val ident: String): LcExpression()
    data class Lambda(val boundVarIdent: String, val body: LcExpression): LcExpression()
    data class Application(val rator: LcExpression, val rand: LcExpression): LcExpression()
}

val lcExpressionScope = LcExpScope<LcExpression, String> (
    varExpr = { LcExpression.Var(it) },
    lambdaExpr = { boundIdent, body -> LcExpression.Lambda(boundIdent, body) },
    appExpr = { rator, rand -> LcExpression.Application(rator, rand) },
    asVarExpr = {
        if (it is LcExpression.Var) { some { it.ident } } else { none() }
    },
    asLambdaExpr = {
        if (it is LcExpression.Lambda) { some { LambdaExpr(it.boundVarIdent, it.body) } } else { none() }
    },
    asAppExpr = {
        if (it is LcExpression.Application) { some {ApplicationExpr(it.rator, it.rand)} } else { none() }
    }
)

fun ex2_15() {
    fun<T> testScope(scope: () -> LcExpScope<T, String>): () -> Unit = {
        with(scope()) {
            val first = appExpr(
                lambdaExpr(
                    "a",
                    appExpr(
                        varExpr("a"),
                        varExpr("b")
                    )
                ),
                varExpr("c")
            )
            println("a occurs free in first? ${occursFree("a", first)}")
            println("b occurs free in first? ${occursFree("b", first)}")
            println("c occurs free in first? ${occursFree("c", first)}")
            println("d occurs free in first? ${occursFree("d", first)}")

            val second = lambdaExpr(
                "x",
                lambdaExpr(
                    "y",
                    appExpr(
                        lambdaExpr(
                            "x",
                            appExpr(
                                varExpr("x"),
                                varExpr("y")
                            )
                        ),
                        varExpr("x")
                    )
                )
            )
            println("x occurs free in second? ${occursFree("x", second)}")
            println("y occurs free in second? ${occursFree("y", second)}")
            println("z occurs free in second? ${occursFree("z", second)}")
        }
    }

    arrayOf(
        "data structure repr" to testScope { lcExpressionScope },
    ).forEach {
        val (label, testScope) = it
        println("testing $label:")
        testScope()
        println()
    }
}
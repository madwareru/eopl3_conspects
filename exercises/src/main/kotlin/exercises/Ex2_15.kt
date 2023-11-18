package exercises

data class LambdaExpr<TLcExp, TVar>(val boundVar: TVar, val body: TLcExp)
data class ApplicationExpr<TLcExp>(val rator: TLcExp, val rand: TLcExp)

data class LcExpScope<TLcExp, TVar> (
    // constructors:
    val varExpr: (TVar) -> TLcExp,
    val lambdaExpr: (TVar, TLcExp) -> TLcExp,
    val appExpr: (TLcExp, TLcExp) -> TLcExp,

    // extractors (predicates aren't needed):
    val asVarExpr: (TLcExp) -> TVar?,
    val asLambdaExpr: (TLcExp) -> LambdaExpr<TLcExp, TVar>?,
    val asAppExpr: (TLcExp) -> ApplicationExpr<TLcExp>?
) {
    fun <TRes> match(
        exp: TLcExp,
        caseVar: (TVar) -> TRes,
        caseLambda: (LambdaExpr<TLcExp, TVar>) -> TRes,
        caseApplication: (ApplicationExpr<TLcExp>) -> TRes
    ): TRes {
        return asVarExpr(exp)?.let(caseVar)
            ?: asLambdaExpr(exp)?.let(caseLambda)
            ?: asAppExpr(exp)?.let(caseApplication)
            ?: throw IllegalArgumentException("exp is neither var, lambda or app")
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
    asVarExpr = { (it as? LcExpression.Var)?.ident },
    asLambdaExpr = { (it as? LcExpression.Lambda)?.let { l -> LambdaExpr(l.boundVarIdent, l.body) } },
    asAppExpr = { (it as? LcExpression.Application)?.let { a -> ApplicationExpr(a.rator, a.rand) } }
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
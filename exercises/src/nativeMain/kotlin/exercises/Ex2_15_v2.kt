package exercises

interface LcExprFactory<TVar, TExpr: LcExpr<TVar>> {
    // variable constructor
    // !this => this
    operator fun TVar.not(): TExpr

    // lambda constructor
    // this % body => LambdaExpr(this, body)
    operator fun TVar.rem(body: TExpr): TExpr

    // application constructor
    // this * rand => ApplicationExpr(this, rand)
    operator fun TExpr.times(rand: TExpr): TExpr
}

interface LcExpr<TVar> {
    fun <TOut> match(
        caseVar: (TVar) -> TOut,
        caseLambda: (LambdaExpr<LcExpr<TVar>, TVar>) -> TOut,
        caseApplication: (ApplicationExpr<LcExpr<TVar>>) -> TOut
    ) : TOut
}

fun <TVar> LcExpr<TVar>.occursFree(searchVar: TVar): Boolean =
    match(
        caseVar = { it == searchVar },
        caseLambda = { it.boundVar != searchVar && it.body.occursFree(searchVar)},
        caseApplication = { it.rator.occursFree(searchVar) || it.rand.occursFree(searchVar) }
    )

object LcExpressionFactory : LcExprFactory<String, LcExpression> {
    override fun String.not(): LcExpression = LcExpression.Var(this)

    override fun String.rem(body: LcExpression): LcExpression = LcExpression.Lambda(this, body)

    override fun LcExpression.times(rand: LcExpression): LcExpression = LcExpression.Application(this, rand)
}

fun ex2_15_v2() {
    fun<TE: LcExpr<String>, TF : LcExprFactory<String, TE>> testScope(scope: () -> TF): () -> Unit = {
        with(scope()) {
            val first = ("a" % (!"a" * !"b")) * !"c"

            println("a occurs free in first? ${first.occursFree("a")}")
            println("b occurs free in first? ${first.occursFree("b")}")
            println("c occurs free in first? ${first.occursFree("c")}")
            println("d occurs free in first? ${first.occursFree("d")}")

            val second =
                ("x" %
                    ("y" %
                        ("x" % (!"x" * !"y")) * !"x"
                    )
                )

            println("x occurs free in second? ${second.occursFree("x")}")
            println("y occurs free in second? ${second.occursFree("y")}")
            println("z occurs free in second? ${second.occursFree("z")}")
        }
    }

    arrayOf(
        "data structure repr" to testScope { LcExpressionFactory },
    ).forEach {
        val (label, testScope) = it
        println("testing $label:")
        testScope()
        println()
    }
}
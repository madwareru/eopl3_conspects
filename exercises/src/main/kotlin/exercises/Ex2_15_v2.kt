package exercises

interface LcExpADT<TImpl, TVar> {
    operator fun TVar.not(): TImpl
    operator fun TVar.rem(body: TImpl): TImpl
    operator fun TImpl.times(rand: TImpl): TImpl
    fun <TOut> TImpl.match(
        caseVar: (TVar) -> TOut,
        caseLambda: (LambdaExpr<TImpl, TVar>) -> TOut,
        caseApplication: (ApplicationExpr<TImpl>) -> TOut
    ): TOut
    fun occursFree(searchVar: TVar, exp: TImpl): Boolean =
        exp.match(
            caseVar = { it == searchVar },
            caseLambda = { it.boundVar != searchVar && occursFree(searchVar, it.body)},
            caseApplication = { occursFree(searchVar, it.rator) || occursFree(searchVar, it.rand) }
        )
}

val lcExpressionADT = object : LcExpADT<LcExpression, String> {
    override operator fun String.not(): LcExpression = LcExpression.Var(this)
    override operator fun String.rem(body: LcExpression): LcExpression =
        LcExpression.Lambda(this, body)
    override operator fun LcExpression.times(rand: LcExpression): LcExpression =
        LcExpression.Application(this, rand)
    override fun <TOut> LcExpression.match(
        caseVar: (String) -> TOut,
        caseLambda: (LambdaExpr<LcExpression, String>) -> TOut,
        caseApplication: (ApplicationExpr<LcExpression>) -> TOut
    ): TOut = when (this) {
        is LcExpression.Var -> caseVar(this.ident)
        is LcExpression.Application -> caseApplication(ApplicationExpr(this.rator, this.rand))
        is LcExpression.Lambda -> caseLambda(LambdaExpr(this.boundVarIdent, this.body))
    }
}

fun ex2_15_v2() {
    fun<T> testScope(scope: () -> LcExpADT<T, String>): () -> Unit = {
        with(scope()) {
            val first = "a" % !"a" * !"b" * !"c"

            println("a occurs free in first? ${occursFree("a", first)}")
            println("b occurs free in first? ${occursFree("b", first)}")
            println("c occurs free in first? ${occursFree("c", first)}")
            println("d occurs free in first? ${occursFree("d", first)}")

            val second =
                ( "x" %
                    ( "y" %
                        ( "x" % !"x" * !"y" * !"x" ) ) )

            println("x occurs free in second? ${occursFree("x", second)}")
            println("y occurs free in second? ${occursFree("y", second)}")
            println("z occurs free in second? ${occursFree("z", second)}")
        }
    }

    arrayOf(
        "data structure repr" to testScope { lcExpressionADT },
    ).forEach {
        val (label, testScope) = it
        println("testing $label:")
        testScope()
        println()
    }
}
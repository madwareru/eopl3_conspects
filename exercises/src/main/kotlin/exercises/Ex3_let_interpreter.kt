package exercises

import com.github.h0tk3y.betterParse.combinators.*
import com.github.h0tk3y.betterParse.grammar.Grammar
import com.github.h0tk3y.betterParse.grammar.parseToEnd
import com.github.h0tk3y.betterParse.grammar.parser
import com.github.h0tk3y.betterParse.lexer.literalToken
import com.github.h0tk3y.betterParse.lexer.regexToken
import com.github.h0tk3y.betterParse.parser.Parser

sealed class LetValue {
    data class Number(val value: Int) : LetValue()
    data class Boolean(val value: kotlin.Boolean) : LetValue()
    companion object {
        fun from(n: Int) = Number(n)
        fun from(b: kotlin.Boolean) = Boolean(b)
    }
}

sealed class LetError<TVar> {
    data class VarNotFound<TVar>(val boundName: TVar) : LetError<TVar>()
    class CantCastBoolToNumber<TVar> : LetError<TVar>()
    class CantCastNumberToBool<TVar> : LetError<TVar>()
}

interface LetEnv<TVar, TEnv: LetEnv<TVar, TEnv>> {
    fun append(binding: Pair<TVar, LetValue>): LetEnvironment<TVar>
    fun append(boundName: TVar, value: Int) = append(boundName to LetValue.from(value))
    fun append(boundName: TVar, value: Boolean) = append(boundName to LetValue.from(value))
    fun get(boundName: TVar): Result<LetValue, LetError<TVar>>
}

interface LetEnvFactory<TVar, TEnv: LetEnv<TVar, TEnv>> {
    fun makeEnv(): TEnv
}

data class LetEnvironment<TVar>(
    val head: Option<Pair<TVar, LetValue>>,
    val tail: Option<LetEnvironment<TVar>>
): LetEnv<TVar, LetEnvironment<TVar>> {
    override fun append(binding: Pair<TVar, LetValue>): LetEnvironment<TVar> =
        LetEnvironment(some { binding }, some{ this })
    override fun get(boundName: TVar): Result<LetValue, LetError<TVar>> = head
        .flatMap { (it.first == boundName).guard { it.second } }
        .okOr { tail.okOr { err { LetError.VarNotFound(boundName) } }.flatMap { it.get(boundName) } }
}

object StringLetEnvironmentFactory: LetEnvFactory<String, LetEnvironment<String>> {
    override fun makeEnv(): LetEnvironment<String> = LetEnvironment(none(), none())
}

sealed class LetExpression<TVar> {
    data class Constant<TVar>(val value: Int) : LetExpression<TVar>()
    data class ZeroCheck<TVar>(val nested: LetExpression<TVar>) : LetExpression<TVar>()
    data class If<TVar>(
        val condition: LetExpression<TVar>,
        val then: LetExpression<TVar>,
        val otherwise: LetExpression<TVar>
    ) : LetExpression<TVar>()
    data class Diff<TVar>(val l: LetExpression<TVar>, val r: LetExpression<TVar>) : LetExpression<TVar>()
    data class Var<TVar>(val boundName: TVar) : LetExpression<TVar>()
    data class Let<TVar>(
        val binding: TVar,
        val value: LetExpression<TVar>,
        val expr: LetExpression<TVar>
    ) : LetExpression<TVar>()
}

fun <TVar, TEnv: LetEnv<TVar, TEnv>> LetExpression<TVar>.value(env: TEnv): Result<LetValue, LetError<TVar>> =
    when (this) {
        is LetExpression.Constant -> ok { LetValue.from(value) }
        is LetExpression.Diff ->
            l.value(env).flatMap { lhs ->
                r.value(env).flatMap { rhs ->
                    if (lhs is LetValue.Number && rhs is LetValue.Number) {
                        ok { LetValue.from(lhs.value - rhs.value) }
                    } else {
                        err { LetError.CantCastBoolToNumber() }
                    }
                }
            }
        is LetExpression.If ->
            condition.value(env).flatMap { cond ->
                cond
                    .tryCast<LetValue.Boolean>()
                    .map { boolCond -> if (boolCond.value) then.value(env) else otherwise.value(env) }
                    .unwrapOr { err { LetError.CantCastNumberToBool() } }
            }
        is LetExpression.Let -> value.value(env).flatMap { v -> expr.value(env.append(binding to v)) }
        is LetExpression.Var -> env.get(boundName)
        is LetExpression.ZeroCheck ->
            nested.value(env).flatMap { v ->
                when (v) {
                    is LetValue.Boolean -> err { LetError.CantCastBoolToNumber() }
                    is LetValue.Number -> ok { LetValue.from(v.value == 0) }
                }
            }
    }

data class LetProgram<TVar>(val expr: LetExpression<TVar>)
fun <TVar, TEnv: LetEnv<TVar, TEnv>> LetProgram<TVar>.value(env: TEnv): Result<LetValue, LetError<TVar>> =
    expr.value(env)

object StringLetProgramGrammar : Grammar<LetProgram<String>>() {
    // scanning
    @Suppress("unused")
    private val _ws by regexToken("\\s+", ignore = true)

    private val letKw by literalToken("let")
    private val inKw by literalToken("in")
    private val ifKw by literalToken("if")
    private val thenKw by literalToken("then")
    private val elseKw by literalToken("else")
    private val zeroCheckKw by literalToken("zero?")
    private val eqSign by literalToken("=")
    private val minusSign by literalToken("-")
    private val comma by literalToken(",")
    private val lParen by literalToken("(")
    private val rParen by literalToken(")")
    private val id by regexToken("([:_]|[a-zA-Z])([:_\\-!?]|[a-zA-Z])*")
    private val number by regexToken("[0-9]+")

    //parsing
    private val parseVarExpr: Parser<LetExpression<String>> by id
        .use { LetExpression.Var(text) }

    private val parseConstantExpr: Parser<LetExpression<String>> by number
        .use { LetExpression.Constant(text.toInt()) }

    private val parseZeroCheckExpr: Parser<LetExpression<String>> by (-zeroCheckKw * parser { parseExpr })
        .map { LetExpression.ZeroCheck(it) }

    private val parseIfExpr: Parser<LetExpression<String>> by
        (-ifKw * parser { parseExpr } * -thenKw * parser { parseExpr } * -elseKw * parser { parseExpr })
            .use { LetExpression.If(t1, t2, t3) }

    private val parseLetExpr: Parser<LetExpression<String>> by
        (-letKw * id * -eqSign * parser { parseExpr } * -inKw * parser { parseExpr })
            .use { LetExpression.Let(t1.text, t2, t3) }

    private val parseDiffExpr: Parser<LetExpression<String>> by
        (-minusSign * -lParen * parser { parseExpr } * -comma * parser { parseExpr } * -rParen)
            .use { LetExpression.Diff(t1, t2) }

    private val parseExpr by
        parseIfExpr or
        parseVarExpr or
        parseConstantExpr or
        parseZeroCheckExpr or
        parseLetExpr or
        parseDiffExpr

    override val rootParser by parseExpr.use { LetProgram(this) }
}

fun ex3_let_interpreter() {
    fun test(source: String): Result<LetValue, LetError<String>> {
        println(source)
        val parsedAst = StringLetProgramGrammar.parseToEnd(source)
        println(parsedAst)

        val env = StringLetEnvironmentFactory
            .makeEnv()
            .append("a", 100)
            .append("b", 21)
            .append("c", true)
            .append("d", 0)

        val result = parsedAst.value(env)
        println(result)
        println()
        return result
    }

    test("a")
    test("zero? a")
    test("b")
    test("zero? a")
    test("c")
    test("zero? c")
    test("d")
    test("zero? d")
    test("e")
    test("zero? e")
    test("if c then 1 else 0")
    test("-(a, b)")
    test("let e = if c then 5 else 10 in -(-(a, e), b)")
    test("let e = zero? 1 in if e then 100500 else zero? 1")
    test(
        """
            zero? 
                let z = 2 in 
                let x = 3 in 
                let y = -(x, 1) in 
                let x = 4 in -(z, -(x, y))
        """.trimIndent()
    )
}
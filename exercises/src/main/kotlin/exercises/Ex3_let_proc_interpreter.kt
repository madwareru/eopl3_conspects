package exercises

import com.github.h0tk3y.betterParse.combinators.*
import com.github.h0tk3y.betterParse.grammar.Grammar
import com.github.h0tk3y.betterParse.grammar.parseToEnd
import com.github.h0tk3y.betterParse.grammar.parser
import com.github.h0tk3y.betterParse.lexer.literalToken
import com.github.h0tk3y.betterParse.lexer.regexToken
import com.github.h0tk3y.betterParse.parser.Parser

sealed class LetProcValue<TVar> {
    data class Number<TVar>(val value: Int) : LetProcValue<TVar>()
    data class Boolean<TVar>(val value: kotlin.Boolean) : LetProcValue<TVar>()
    sealed class Proc<TVar> : LetProcValue<TVar>()
    {
        data class Intrinsic<TVar>(
            val arguments: List<TVar>,
            val env: LetProcEnvironment<TVar>,
            val body: (LetProcEnvironment<TVar>) -> Result<LetProcValue<TVar>, LetProcError<TVar>>
        ) : Proc<TVar>()
        data class User<TVar>(
            val recursiveName: Option<TVar>,
            val arguments: List<TVar>,
            val env: LetProcEnvironment<TVar>,
            val body: LetProcExpression<TVar>
        ) : Proc<TVar>()
    }

    companion object {
        fun<TVar> from(n: Int) = Number<TVar>(n)
        fun<TVar> from(b: kotlin.Boolean) = Boolean<TVar>(b)
    }
}

fun<TVar> LetProcValue<TVar>.printed(): String = when (this) {
    is LetProcValue.Boolean -> if (value) "#t" else "#f"
    is LetProcValue.Number -> value.toString()
    is LetProcValue.Proc.Intrinsic -> "intrinsic proc. Args: ${arguments.joinToString(", ") { it.toString() }}"
    is LetProcValue.Proc.User -> "user proc. Args: ${arguments.joinToString(", ") { it.toString() }}"
}

fun <TVar> LetProcValue<TVar>.structuralEq(other: LetProcValue<TVar>): Result<LetProcValue<TVar>, LetProcError<TVar>> {
    return when {
        this is LetProcValue.Number<TVar> && other is LetProcValue.Number<TVar> ->
            ok { LetProcValue.from(this.value == other.value) }
        this is LetProcValue.Boolean<TVar> && other is LetProcValue.Boolean<TVar> ->
            ok { LetProcValue.from(this.value == other.value) }
        else -> err { LetProcError.CantCompareTypes(this, other) }
    }
}

fun <TVar> LetProcValue.Proc.Intrinsic<TVar>.eval(bindings: List<LetProcValue<TVar>>) = when {
    arguments.size == bindings.size -> body(arguments.zip(bindings).fold(env) { newEnv, next -> newEnv.append(next) })
    arguments.size < bindings.size -> {
        val partialBindings = mutableListOf<LetProcValue<TVar>>()
        val restBindings = mutableListOf<LetProcExpression<TVar>>()
        for (i in bindings.indices) {
            when {
                i < arguments.size -> partialBindings.add(bindings[i])
                else -> restBindings.add(LetProcExpression.Value(bindings[i]))
            }
        }
        val newEnv = arguments.zip(partialBindings).fold(env) { newEnv, next -> newEnv.append(next) }
        body(newEnv).flatMap { LetProcExpression.Call(LetProcExpression.Value(it), restBindings).eval(newEnv) }
    }
    else -> {
        val partialArguments = mutableListOf<TVar>()
        val restArguments = mutableListOf<TVar>()
        for (i in arguments.indices) {
            when {
                i < bindings.size -> partialArguments.add(arguments[i])
                else -> restArguments.add(arguments[i])
            }
        }
        val newEnv = partialArguments.zip(bindings).fold(env) { newEnv, next -> newEnv.append(next) }
        ok { LetProcValue.Proc.Intrinsic(restArguments, newEnv) { nestedEnv ->
            val fullBindings = mutableListOf<LetProcExpression<TVar>>()
            for (b in bindings) fullBindings.add(LetProcExpression.Value(b))

            for (arg in restArguments) {
                when (val b = nestedEnv.get(arg)) {
                    is Option.None -> return@Intrinsic err { LetProcError.NotFound(arg) }
                    is Option.Some -> fullBindings.add(LetProcExpression.Value(b.value))
                }
            }

            LetProcExpression.Call(LetProcExpression.Value(this), fullBindings).eval(nestedEnv)
        } }
    }
}

fun <TVar> LetProcValue.Proc.User<TVar>.eval(bindings: List<LetProcValue<TVar>>) = when {
    arguments.size == bindings.size -> {
        val newEnv = arguments.zip(bindings).fold(env) { newEnv, next -> newEnv.append(next) }
        body.eval( recursiveName.map { newEnv.append(it to this) }.unwrapOr { newEnv })
    }
    arguments.size < bindings.size -> {
        val partialBindings = mutableListOf<LetProcValue<TVar>>()
        val restBindings = mutableListOf<LetProcExpression<TVar>>()
        for (i in bindings.indices) {
            when {
                i < arguments.size -> partialBindings.add(bindings[i])
                else -> restBindings.add(LetProcExpression.Value(bindings[i]))
            }
        }
        val newEnv = arguments.zip(partialBindings).fold(env) { newEnv, next -> newEnv.append(next) }
        val newEnvEnsured = recursiveName.map { newEnv.append(it to this) }.unwrapOr { newEnv }
        body.eval(newEnvEnsured)
            .flatMap {
                LetProcExpression.Call(LetProcExpression.Value(it), restBindings).eval(newEnvEnsured)
            }
    }
    else -> {
        val partialArguments = mutableListOf<TVar>()
        val restArguments = mutableListOf<TVar>()
        for (i in arguments.indices) {
            when {
                i < bindings.size -> partialArguments.add(arguments[i])
                else -> restArguments.add(arguments[i])
            }
        }
        val newEnv = partialArguments.zip(bindings).fold(env) { newEnv, next -> newEnv.append(next) }
        val newEnvEnsured = recursiveName.map { newEnv.append(it to this) }.unwrapOr { newEnv }
        ok { LetProcValue.Proc.Intrinsic(restArguments, newEnvEnsured) { nestedEnv ->
            val fullBindings = mutableListOf<LetProcExpression<TVar>>()
            for (b in bindings) fullBindings.add(LetProcExpression.Value(b))

            for (arg in restArguments) {
                when (val b = nestedEnv.get(arg)) {
                    is Option.None -> return@Intrinsic err { LetProcError.NotFound(arg) }
                    is Option.Some -> fullBindings.add(LetProcExpression.Value(b.value))
                }
            }

            LetProcExpression.Call(LetProcExpression.Value(this), fullBindings).eval(nestedEnv)
        } }
    }
}

sealed class LetProcError<TVar> {
    data class NotFound<TVar>(val boundName: TVar) : LetProcError<TVar>()
    data class CantCastToNumber<TVar>(val v: LetProcValue<TVar>) : LetProcError<TVar>()
    data class CantCastToBool<TVar>(val v: LetProcValue<TVar>) : LetProcError<TVar>()
    data class CantCastToProc<TVar>(val v: LetProcValue<TVar>) : LetProcError<TVar>()
    data class CantBindValueToRecursive<TVar>(val v: LetProcValue<TVar>) : LetProcError<TVar>()
    data class CantCompareTypes<TVar>(val l: LetProcValue<TVar>, val r: LetProcValue<TVar>) : LetProcError<TVar>()
    data class CantCallOnNonProcValue<TVar>(val value: LetProcValue<TVar>) : LetProcError<TVar>()
}

interface LetProcEnv<TVar, TEnv: LetProcEnv<TVar, TEnv>> {
    fun append(binding: Pair<TVar, LetProcValue<TVar>>): LetProcEnv<TVar, TEnv>
    fun get(boundName: TVar): Option<LetProcValue<TVar>>
}

interface LetProcEnvFactory<TVar, TEnv: LetProcEnvironment<TVar>> {
    fun makeEnv(): TEnv
}

data class LetProcEnvironment<TVar>(
    val head: Option<Pair<TVar, LetProcValue<TVar>>>,
    val tail: Option<LetProcEnvironment<TVar>>
): LetProcEnv<TVar, LetProcEnvironment<TVar>> {
    override fun append(binding: Pair<TVar, LetProcValue<TVar>>): LetProcEnvironment<TVar> =
        LetProcEnvironment(some { binding }, some{ this })
    override fun get(boundName: TVar): Option<LetProcValue<TVar>> = head
        .flatMap { (it.first == boundName).guard { it.second } }
        .or { tail.flatMap { it.get(boundName) } }
}

object StringLetProcEnvironmentFactory: LetProcEnvFactory<String, LetProcEnvironment<String>> {
    override fun makeEnv(): LetProcEnvironment<String> = LetProcEnvironment(none(), none())
}

sealed class LetProcExpression<TVar> {
    data class Value<TVar>(val value: LetProcValue<TVar>) : LetProcExpression<TVar>()
    data class Name<TVar>(val name: TVar) : LetProcExpression<TVar>()
    data class Lambda<TVar>(val arguments: List<TVar>, val body: LetProcExpression<TVar>) : LetProcExpression<TVar>()
    data class If<TVar>(
        val condition: LetProcExpression<TVar>,
        val then: LetProcExpression<TVar>,
        val otherwise: LetProcExpression<TVar>
    ) : LetProcExpression<TVar>()
    data class Let<TVar>(
        val recursive: Boolean,
        val binding: Pair<TVar, LetProcExpression<TVar>>,
        val expr: LetProcExpression<TVar>
    ) : LetProcExpression<TVar>()
    data class Call<TVar>(
        val callee: LetProcExpression<TVar>,
        val args: List<LetProcExpression<TVar>>
    ) : LetProcExpression<TVar>()
}

fun <TVar> LetProcExpression<TVar>.eval(env: LetProcEnvironment<TVar>): Result<LetProcValue<TVar>, LetProcError<TVar>> =
    when (this) {
        is LetProcExpression.Value -> ok { value }
        is LetProcExpression.Name -> env.get(name).okOr { err { LetProcError.NotFound(name) } }
        is LetProcExpression.Lambda -> ok { LetProcValue.Proc.User(none(), arguments, env, body) }
        is LetProcExpression.If ->
            condition.eval(env).flatMap { cond ->
                cond
                    .tryCast<LetProcValue.Boolean<TVar>>()
                    .okOr<LetProcValue.Boolean<TVar>, LetProcError<TVar>> { err { LetProcError.CantCastToBool(cond) } }
                    .flatMap { boolCond -> if (boolCond.value) then.eval(env) else otherwise.eval(env) }
            }
        is LetProcExpression.Let ->
            binding.second.eval(env)
                .flatMap { v ->
                    when {
                        recursive ->
                            when {
                                v is LetProcValue.Proc.User -> ok {
                                    LetProcValue.Proc.User(some { binding.first }, v.arguments, v.env, v.body)
                                }
                                else -> err { LetProcError.CantBindValueToRecursive(v) }
                            }
                        else -> ok { v }
                    }
                }
                .flatMap { v -> expr.eval(env.append(binding.first to v)) }
        is LetProcExpression.Call -> {
            val arguments = args.map { arg -> arg.eval(env) }
            when (val argError = arguments.firstOrNone { it is Result.Err }) {
                is Option.None -> callee.eval(env).flatMap { proc ->
                    val safeArguments = arguments.map { (it as Result.Ok).value }
                    when (proc) {
                        is LetProcValue.Proc.Intrinsic -> proc.eval(safeArguments)
                        is LetProcValue.Proc.User -> proc.eval(safeArguments)
                        else -> err { LetProcError.CantCallOnNonProcValue(proc) }
                    }
                }
                is Option.Some -> argError.value
            }
        }
    }

data class LetProcProgram<TVar>(val expr: LetProcExpression<TVar>)
fun <TVar> LetProcProgram<TVar>.eval(env: LetProcEnvironment<TVar>): Result<LetProcValue<TVar>, LetProcError<TVar>> =
    expr.eval(env)

val emptyEnv = StringLetProcEnvironmentFactory.makeEnv()

fun makeBinOp(
    foo: (LetProcValue<String>, LetProcValue<String>) -> Result<LetProcValue<String>, LetProcError<String>>
) = LetProcValue.Proc.Intrinsic(listOf("__lhs__", "__rhs__"), emptyEnv) { env ->
    env.get("__lhs__").okOr { err { LetProcError.NotFound("__lhs__") } }.flatMap { lhs ->
        env.get("__rhs__").okOr { err { LetProcError.NotFound("__rhs__") } }.flatMap { rhs ->
            foo(lhs, rhs)
        }
    }
}

fun makeUnaryOp(
    foo: (LetProcValue<String>) -> Result<LetProcValue<String>, LetProcError<String>>
) = LetProcValue.Proc.Intrinsic(listOf("__operand__"), emptyEnv) { env ->
    env.get("__operand__")
        .okOr { err { LetProcError.NotFound("__operand__") } }
        .flatMap(foo)
}

fun makeEnvWithIntrinsics() = emptyEnv
    .append("identity" to makeUnaryOp { op -> ok { op } } )
    .append(
        "add" to makeBinOp { lhs, rhs -> when {
            lhs !is LetProcValue.Number -> err { LetProcError.CantCastToNumber(lhs) }
            rhs !is LetProcValue.Number -> err { LetProcError.CantCastToNumber(rhs) }
            else -> ok { LetProcValue.from(lhs.value + rhs.value) }
        } }
    ).append(
        "sub" to makeBinOp { lhs, rhs -> when {
            lhs !is LetProcValue.Number -> err { LetProcError.CantCastToNumber(lhs) }
            rhs !is LetProcValue.Number -> err { LetProcError.CantCastToNumber(rhs) }
            else -> ok { LetProcValue.from(lhs.value - rhs.value) }
        } }
    ).append(
        "mul" to makeBinOp { lhs, rhs -> when {
            lhs !is LetProcValue.Number -> err { LetProcError.CantCastToNumber(lhs) }
            rhs !is LetProcValue.Number -> err { LetProcError.CantCastToNumber(rhs) }
            else -> ok { LetProcValue.from(lhs.value * rhs.value) }
        } }
    ).append(
        "div" to makeBinOp { lhs, rhs -> when {
            lhs !is LetProcValue.Number -> err { LetProcError.CantCastToNumber(lhs) }
            rhs !is LetProcValue.Number -> err { LetProcError.CantCastToNumber(rhs) }
            else -> ok { LetProcValue.from(lhs.value / rhs.value) }
        } }
    ).append(
        "less?" to makeBinOp { lhs, rhs -> when {
            lhs !is LetProcValue.Number -> err { LetProcError.CantCastToNumber(lhs) }
            rhs !is LetProcValue.Number -> err { LetProcError.CantCastToNumber(rhs) }
            else -> ok { LetProcValue.from(lhs.value < rhs.value) }
        } }
    ).append(
        "greater?" to makeBinOp { lhs, rhs -> when {
            lhs !is LetProcValue.Number -> err { LetProcError.CantCastToNumber(lhs) }
            rhs !is LetProcValue.Number -> err { LetProcError.CantCastToNumber(rhs) }
            else -> ok { LetProcValue.from(lhs.value > rhs.value) }
        } }
    ).append(
        "and" to makeBinOp { lhs, rhs -> when {
            lhs !is LetProcValue.Boolean -> err { LetProcError.CantCastToBool(lhs) }
            rhs !is LetProcValue.Boolean -> err { LetProcError.CantCastToBool(rhs) }
            else -> ok { LetProcValue.from(lhs.value and rhs.value) }
        } }
    ).append(
        "or" to makeBinOp { lhs, rhs -> when {
            lhs !is LetProcValue.Boolean -> err { LetProcError.CantCastToBool(lhs) }
            rhs !is LetProcValue.Boolean -> err { LetProcError.CantCastToBool(rhs) }
            else -> ok { LetProcValue.from(lhs.value or rhs.value) }
        } }
    ).append(
        "xor" to makeBinOp { lhs, rhs -> when {
            lhs !is LetProcValue.Boolean -> err { LetProcError.CantCastToBool(lhs) }
            rhs !is LetProcValue.Boolean -> err { LetProcError.CantCastToBool(rhs) }
            else -> ok { LetProcValue.from(lhs.value xor rhs.value) }
        } }
    ).append(
        "not" to makeUnaryOp { operand -> when {
            operand !is LetProcValue.Boolean -> err { LetProcError.CantCastToBool(operand) }
            else -> ok { LetProcValue.from(!operand.value) }
        } }
    ).append(
        "minus" to makeUnaryOp { operand -> when {
            operand !is LetProcValue.Number -> err { LetProcError.CantCastToNumber(operand) }
            else -> ok { LetProcValue.from(-operand.value) }
        } }
    ).append(
        "eq?" to makeBinOp { lhs, rhs -> lhs.structuralEq(rhs) }
    ).append(
        "print" to makeUnaryOp { operand ->
            println(operand.printed())
            ok { operand }
        }
    ).append(
        "call-self" to LetProcValue.Proc.Intrinsic(listOf("__proc__"), emptyEnv) { env ->
            env.get("__proc__")
                .okOr { err { LetProcError.NotFound("__proc__") } }
                .flatMap { proc ->
                    when {
                        proc !is LetProcValue.Proc -> err { LetProcError.CantCastToProc(proc) }
                        else -> {
                            val procExpr = LetProcExpression.Value(proc)
                            LetProcExpression.Call(procExpr, listOf(procExpr)).eval(env)
                        }
                    }
                }
        }
    )

object StringLetProcProgramGrammar : Grammar<LetProcProgram<String>>() {
    // scanning
    @Suppress("unused")
    private val _ws by regexToken("\\s+", ignore = true)

    private val letKw by literalToken("let")
    private val recKw by literalToken("rec")
    private val inKw by literalToken("in")
    private val ifKw by literalToken("if")
    private val thenKw by literalToken("then")
    private val elseKw by literalToken("else")
    private val arrowSign by literalToken("->")
    private val eqSign by literalToken("=")
    private val lParen by literalToken("(")
    private val rParen by literalToken(")")
    private val lCurly by literalToken("{")
    private val rCurly by literalToken("}")
    private val id by regexToken("([:_]|[a-zA-Z])([a-zA-Z]|[0-9]|[:_\\-!?])*")
    private val trueLit by literalToken("#t")
    private val falseLit by literalToken("#f")
    private val number by regexToken("[0-9]+")

    //parsing
    private val parseTrueLiteralExpr: Parser<LetProcExpression<String>> by trueLit
        .use { LetProcExpression.Value(LetProcValue.from(true)) }

    private val parseFalseLiteralExpr: Parser<LetProcExpression<String>> by falseLit
        .use { LetProcExpression.Value(LetProcValue.from(false)) }

    private val parseNumberConstantExpr: Parser<LetProcExpression<String>> by number
        .use { LetProcExpression.Value(LetProcValue.from(text.toInt())) }

    private val parseLambdaExpr: Parser<LetProcExpression<String>> by
        (
            -lCurly *
            zeroOrMore(id) *
            -arrowSign *
            parser { parseExpr } *
            -rCurly
        ).use { LetProcExpression.Lambda(t1.map { it.text }, t2) }

    private val parseNameExpr: Parser<LetProcExpression<String>> by id
        .use { LetProcExpression.Name(text) }

    private val parseCallExpr: Parser<LetProcExpression<String>> by
        (
            -lParen *
            parser { parseExpr } *
            zeroOrMore(parser { parseExpr }) *
            -rParen
        ).use { LetProcExpression.Call(t1, t2) }

    private val parseIfExpr: Parser<LetProcExpression<String>> by
    (
        -ifKw *
        parser { parseExpr } *
        -thenKw *
        parser { parseExpr } *
        -elseKw *
        parser { parseExpr }
    ).use { LetProcExpression.If(t1, t2, t3) }

    private val parseLetExpr: Parser<LetProcExpression<String>> by
    (
        -letKw *
        optional(recKw) *
        id *
        -eqSign *
        parser { parseExpr } *
        -inKw *
        parser { parseExpr }
    ).use { LetProcExpression.Let( t1 != null,t2.text to t3, t4) }

    private val parseExpr by
        parseNumberConstantExpr or
        parseTrueLiteralExpr or
        parseFalseLiteralExpr or
        parseNameExpr or
        parseCallExpr or
        parseIfExpr or
        parseLetExpr or
        parseLambdaExpr

    override val rootParser by parseExpr.use { LetProcProgram(this) }
}

fun ex3_let_proc_interpreter() {
    fun test(source: String): Result<LetProcValue<String>, LetProcError<String>> {
        println(source)
        val parsedAst = StringLetProcProgramGrammar.parseToEnd(source)
        println(parsedAst)

        val result = parsedAst.eval(makeEnvWithIntrinsics())
        println(result)
        println()
        return result
    }
    test("( print ( or #f #f ) )")
    test("( print ( or #f #t ) )")
    test("( print ( or #t #f ) )")
    test("( print ( or #t #t ) )")
    test("( print ( and #f #f ) )")
    test("( print ( and #f #t ) )")
    test("( print ( and #t #f ) )")
    test("( print ( and #t #t ) )")
    test("( print ( eq? 1 0 ) )")
    test("( print ( eq? 0 0 ) )")
    test("( print ( add 3 4 ) )")
    test("( print ( sub 3 4 ) )")
    test("( print ( mul 3 4 ) )")
    test("( print ( div 20 3 ) )")
    test("( print ( mul 3 4 ) )")
    test("""
        let add2 = { x y -> ( add x y ) } in 
        ( print ( add2 3 4 ) )
        """.trimIndent()
    )
    test("""
        let carried-add = { x -> if ( eq? x 0 ) then identity else { y -> ( add x y ) } } in 
        ( print ( ( carried-add 3 ) 4 ) )
        """.trimIndent()
    )
    test("""
        let carried-or = { x -> if x then { _ -> #t } else identity } in 
        let carried-and = { x -> if ( not x ) then { _ -> #f } else identity } in 
        ( print ( ( carried-and #f ) ( (carried-or #t ) #f ) ) )
        """.trimIndent()
    )
    test("""
        let print-true = { -> ( print #t ) } in 
        ( print-true )
        """.trimIndent()
    )
    test("""
        let f = { x -> ( sub x 11 ) } in
        ( f ( f 77) )
        """.trimIndent() // returns 55
    )
    test("""
        ( { f -> ( f ( f 77) ) } { x -> ( sub x 11 ) } )
        """.trimIndent() // returns 55
    )
    test(
        """
        let make-mult = { maker -> 
            { x -> 
                if (eq? 0 x) 
                    then 0
                    else ( add ( ( call-self maker ) ( sub x 1 ) ) 4 )
            } 
        } in
        let times4 = ( call-self make-mult ) in
        (times4 3)
        """.trimIndent() // returns 12
    )
    test(
        """
        let one-or-less? = { n -> ( not ( greater? n 1 ) ) } in
        let pred = { n -> ( sub n 1 ) } in
        let factorial = (
            call-self
            { maker ->
                { n -> 
                    if ( one-or-less? n ) 
                        then n else 
                        ( mul n ( ( call-self maker ) ( pred n ) ) ) 
                }
            }
        ) in
        ( print ( factorial 10 ) )
        """.trimIndent() // prints 3628800
    )
    test("""
        let one-or-less? = { n -> ( not ( greater? n 1 ) ) } in
        let pred = { n -> ( sub n 1 ) } in
        let rec factorial = { n -> 
            if ( one-or-less? n ) 
                then n 
                else ( mul n ( factorial ( pred n ) ) ) 
        } in 
        ( print ( factorial 10 ) )
        """.trimIndent() // prints 3628800
    )
    test(
        """
        let add-five = ( add 5 ) in
        (add-five 20)
        """.trimIndent()
    )
    test(
        """
        let add2 = ( add ) in
        (add2 5 20)
        """.trimIndent()
    )
    test(
        """
        let carried-add-three = { x -> { y -> { z -> (add ( add x y ) z ) } } } in
        (carried-add-three 1 2 3)
        """.trimIndent()
    )
    test(
        """
        let rec times = { x y -> 
            if ( less? y 0 ) then ( times x ( minus y ) ) 
            else if ( eq? y 0 ) then 0 
            else ( add x ( times x ( sub y 1 ) ) ) 
        } in
        let times-three = ( times 3 ) in
        ( times-three 5 )
        """.trimIndent()
    )
}
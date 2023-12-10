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
    sealed class List : LetValue() {
        data object Empty : List()
        data class Cons(val head: LetValue, val tail: List) : List()
    }
    companion object {
        fun from(n: Int) = Number(n)
        fun from(b: kotlin.Boolean) = Boolean(b)
    }
}

fun LetValue.printed(): String = when (this) {
    is LetValue.Boolean -> if (value) "#t" else "#f"
    is LetValue.Number -> value.toString()
    is LetValue.List.Cons -> {
        var s = "[ ${head.printed()}"
        var rest = tail
        while (rest is LetValue.List.Cons) {
            s += ", ${rest.head.printed()}"
            rest = rest.tail
        }
        s += " ]"
        s
    }
    LetValue.List.Empty -> "[ ]"
}

fun <TVar> LetValue.structuralEq(other: LetValue): Result<LetValue, LetError<TVar>> {
    return when {
        this is LetValue.Number && other is LetValue.Number -> ok { LetValue.from(this.value == other.value) }
        this is LetValue.Boolean && other is LetValue.Boolean -> ok { LetValue.from(this.value == other.value) }
        this is LetValue.List.Empty && other is LetValue.List.Empty -> ok { LetValue.from(true) }
        this is LetValue.List.Cons && other is LetValue.List.Cons ->
            (this.head.structuralEq<TVar>(other.head)).flatMap { this.tail.structuralEq(other.tail) }
        else -> err { LetError.CantCompareTypes(this, other) }
    }
}

infix fun LetValue.cons(tail: LetValue.List): LetValue.List = LetValue.List.Cons(this, tail)
infix fun LetValue.List.concat(other: LetValue.List): LetValue.List {
    tailrec fun concatImpl(
        left: LetValue.List,
        right: LetValue.List,
        continuation: (LetValue.List) -> LetValue.List
    ): LetValue.List = when (left) {
        LetValue.List.Empty -> continuation(right)
        is LetValue.List.Cons -> concatImpl(left.tail, right) { continuation(left.head cons it) }
    }

    return concatImpl(this, other) { it }
}
val LetValue.List.length get(): Int {
    tailrec fun lengthImpl(l: LetValue.List, acc: Int): Int = when (l) {
        LetValue.List.Empty -> acc
        is LetValue.List.Cons -> lengthImpl(l.tail, 1 + acc)
    }
    return lengthImpl(this, 0)
}

sealed class LetError<TVar> {
    data class VarNotFound<TVar>(val boundName: TVar) : LetError<TVar>()
    class CantCastToNumber<TVar> : LetError<TVar>()
    class CantCastToBool<TVar> : LetError<TVar>()
    class CantCastToList<TVar> : LetError<TVar>()
    class CantCarFromEmptyList<TVar> : LetError<TVar>()
    data class CantCompareTypes<TVar>(val l: LetValue, val r: LetValue) : LetError<TVar>()
    data class UnsupportedArithmeticOperator<TVar>(val op: BinaryOperator): LetError<TVar>()
    data class UnsupportedListOperator<TVar>(val op: BinaryOperator): LetError<TVar>()
    data class UnsupportedBooleanOperator<TVar>(val op: BinaryOperator): LetError<TVar>()
    data class UnsupportedComparisonOperator<TVar>(val op: BinaryOperator): LetError<TVar>()
    data class UnsupportedOperator<TVar>(val op: BinaryOperator): LetError<TVar>()
    class NoSuitableCondMatchFound<TVar> : LetError<TVar>()
}

interface LetEnv<TVar, TEnv: LetEnv<TVar, TEnv>> {
    fun append(binding: Pair<TVar, LetValue>): TEnv
    fun get(boundName: TVar): Option<LetValue>
}

fun<TVar, TEnv: LetEnv<TVar, TEnv>> LetEnv<TVar, TEnv>.append(boundName: TVar, value: Int) =
    append(boundName to LetValue.from(value))
fun<TVar, TEnv: LetEnv<TVar, TEnv>> LetEnv<TVar, TEnv>.append(boundName: TVar, value: Boolean) =
    append(boundName to LetValue.from(value))

interface LetEnvFactory<TVar, TEnv: LetEnv<TVar, TEnv>> {
    fun makeEnv(): TEnv
}

data class LetEnvironment<TVar>(
    val head: Option<Pair<TVar, LetValue>>,
    val tail: Option<LetEnvironment<TVar>>
): LetEnv<TVar, LetEnvironment<TVar>> {
    override fun append(binding: Pair<TVar, LetValue>): LetEnvironment<TVar> =
        LetEnvironment(some { binding }, some{ this })
    override fun get(boundName: TVar): Option<LetValue> = head
        .flatMap { (it.first == boundName).guard { it.second } }
        .or { tail.flatMap { it.get(boundName) } }
}

object StringLetEnvironmentFactory: LetEnvFactory<String, LetEnvironment<String>> {
    override fun makeEnv(): LetEnvironment<String> = LetEnvironment(none(), none())
}

sealed class UnaryOperator {
    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                          *** eval(operand: LetExpression, env: LetEnv) calculation ***                        |
     +===============================================================================================================+
     |                                                                                                               |
     |                                     opValue = operand.eval(env) ?!                                            |
     |  -----------------------------------------------------------------------------------------------------------  |
     |      opValue !is list                     Empty = opValue                     Cons(head, tail) = opValue      |
     |  ------------------------          -------------------------------          --------------------------------  |
     |   Err(Can't cast to list)           Err(Can't car from empty list)                      Ok(head)              |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Car: UnaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                          *** eval(operand: LetExpression, env: LetEnv) calculation ***                        |
     +===============================================================================================================+
     |                                                                                                               |
     |                                     opValue = operand.eval(env) ?!                                            |
     |  -----------------------------------------------------------------------------------------------------------  |
     |      opValue !is list                     Empty = opValue                     Cons(head, tail) = opValue      |
     |  ------------------------          -------------------------------          --------------------------------  |
     |   Err(Can't cast to list)           Err(Can't car from empty list)                      Ok(tail)              |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Cdr: UnaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                          *** eval(operand: LetExpression, env: LetEnv) calculation ***                        |
     +===============================================================================================================+
     |                                                                                                               |
     |                                     opValue = operand.eval(env) ?!                                            |
     |                       -------------------------------------------------------                                 |
     |                              opValue !is list             opValue is list                                     |
     |                         ---------------------------    ---------------------                                  |
     |                           Err(Can't cast to list)       Ok(Empty = opValue)                                   |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Empty: UnaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                          *** eval(operand: LetExpression, env: LetEnv) calculation ***                        |
     +===============================================================================================================+
     |                                                                                                               |
     |                                     opValue = operand.eval(env) ?!                                            |
     |                       -------------------------------------------------------                                 |
     |                              opValue !is list             opValue is list                                     |
     |                         ---------------------------     --------------------                                  |
     |                           Err(Can't cast to list)        Ok(opValue.length)                                   |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Length: UnaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                          *** eval(operand: LetExpression, env: LetEnv) calculation ***                        |
     +===============================================================================================================+
     |                                                                                                               |
     |                                     opValue = operand.eval(env) ?!                                            |
     |                       -------------------------------------------------------                                 |
     |                              opValue !is number          opValue is number                                    |
     |                         ---------------------------    ---------------------                                  |
     |                          Err(Can't cast to number)          Ok(-opValue)                                      |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Minus: UnaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                          *** eval(operand: LetExpression, env: LetEnv) calculation ***                        |
     +===============================================================================================================+
     |                                                                                                               |
     |                                        opValue = operand.eval(env) ?!                                         |
     |                          ---------------------------------------------------------                            |
     |                              opValue !is number             opValue is number                                 |
     |                            ---------------------------    ----------------------                              |
     |                             Err(Can't cast to number)        Ok(opValue == 0)                                 |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object IsZero: UnaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                          *** eval(operand: LetExpression, env: LetEnv) calculation ***                        |
     +===============================================================================================================+
     |                                                                                                               |
     |                                        opValue = operand.eval(env) ?!                                         |
     |       ------------------------------------------------------------------------------------------------        |
     |         opValue !is number && opValue !is boolean        opValue is boolean        opValue is number          |
     |        -------------------------------------------   -------------------------    -------------------         |
     |                 Err(Can't cast to boolean)                  Ok(opValue)             Ok(opValue != 0)          |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object AsBool: UnaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                          *** eval(operand: LetExpression, env: LetEnv) calculation ***                        |
     +===============================================================================================================+
     |                                                                                                               |
     |                                           opValue = operand.eval(env) ?!                                      |
     |                             -------------------------------------------------------                           |
     |                                 opValue !is boolean          opValue is boolean                               |
     |                               -------------------------    ----------------------                             |
     |                                Err(Can't cast to bool)          Ok(!opValue)                                  |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Not: UnaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                          *** eval(operand: LetExpression, env: LetEnv) calculation ***                        |
     +===============================================================================================================+
     |                                                                                                               |
     |                                        opValue = operand.eval(env) ?!                                         |
     |       ------------------------------------------------------------------------------------------------        |
     |         opValue !is number && opValue !is boolean        opValue is boolean        opValue is number          |
     |        -------------------------------------------   -------------------------    -------------------         |
     |                  Err(Can't cast to number)            Ok(if opValue 1 else 0)          Ok(opValue)            |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object AsNumber: UnaryOperator()
}

sealed class BinaryOperator {
    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                     *** eval(l: LetExpression, r: LetExpression, env: LetEnv) calculation ***                 |
     +===============================================================================================================+
     |                                                                                                               |
     |                                            lv = l.eval(env) ?!                                                |
     |                     ---------------------------------------------------------------------                     |
     |                                            rv = r.eval(env) ?!                                                |
     |                      -------------------------------------------------------------------                      |
     |                        lv and rv are not of sane type      lv and rv are of same type                         |
     |                       --------------------------------   ------------------------------                       |
     |                          Err(Can't cast to boolean)         Ok(lv.structuralEq(rv))                            |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Eq : BinaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                     *** eval(l: LetExpression, r: LetExpression, env: LetEnv) calculation ***                 |
     +===============================================================================================================+
     |                                                                                                               |
     |                                            lv = l.eval(env) ?!                                                |
     |                     ---------------------------------------------------------------------                     |
     |                                            rv = r.eval(env) ?!                                                |
     |                      -------------------------------------------------------------------                      |
     |                       lv !is boolean || rv !is boolean   lv is boolean && rv is boolean                       |
     |                       --------------------------------   ------------------------------                       |
     |                          Err(Can't cast to boolean)               Ok(lv && rv)                                |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object And : BinaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                     *** eval(l: LetExpression, r: LetExpression, env: LetEnv) calculation ***                 |
     +===============================================================================================================+
     |                                                                                                               |
     |                                            lv = l.eval(env) ?!                                                |
     |                     ---------------------------------------------------------------------                     |
     |                                            rv = r.eval(env) ?!                                                |
     |                      -------------------------------------------------------------------                      |
     |                       lv !is boolean || rv !is boolean   lv is boolean && rv is boolean                       |
     |                       --------------------------------   ------------------------------                       |
     |                          Err(Can't cast to boolean)               Ok(lv || rv)                                |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Or : BinaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                     *** eval(l: LetExpression, r: LetExpression, env: LetEnv) calculation ***                 |
     +===============================================================================================================+
     |                                                                                                               |
     |                                            lv = l.eval(env) ?!                                                |
     |                     ---------------------------------------------------------------------                     |
     |                                            rv = r.eval(env) ?!                                                |
     |                      -------------------------------------------------------------------                      |
     |                       lv !is boolean || rv !is boolean   lv is boolean && rv is boolean                       |
     |                       --------------------------------   ------------------------------                       |
     |                          Err(Can't cast to boolean)               Ok(lv ^ rv)                                 |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Xor : BinaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                     *** eval(l: LetExpression, r: LetExpression, env: LetEnv) calculation ***                 |
     +===============================================================================================================+
     |                                                                                                               |
     |                                            lv = l.eval(env) ?!                                                |
     |                     ---------------------------------------------------------------------                     |
     |                                            rv = r.eval(env) ?!                                                |
     |                      -------------------------------------------------------------------                      |
     |                        lv !is number || rv !is number     lv is number && rv is number                        |
     |                       --------------------------------   ------------------------------                       |
     |                          Err(Can't cast to number)                Ok(lv - rv)                                 |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Subtract : BinaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                     *** eval(l: LetExpression, r: LetExpression, env: LetEnv) calculation ***                 |
     +===============================================================================================================+
     |                                                                                                               |
     |                                            lv = l.eval(env) ?!                                                |
     |                     ---------------------------------------------------------------------                     |
     |                                            rv = r.eval(env) ?!                                                |
     |                      -------------------------------------------------------------------                      |
     |                                rv !is list                        rv is list                                  |
     |                       --------------------------------   ------------------------------                       |
     |                          Err(Can't cast to list)                Ok(lv cons rv)                                |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Cons : BinaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                     *** eval(l: LetExpression, r: LetExpression, env: LetEnv) calculation ***                 |
     +===============================================================================================================+
     |                                                                                                               |
     |                                            lv = l.eval(env) ?!                                                |
     |                     ---------------------------------------------------------------------                     |
     |                                            rv = r.eval(env) ?!                                                |
     |                      -------------------------------------------------------------------                      |
     |                        lv !is list || rv !is list                rv is list                                   |
     |                       --------------------------------   ------------------------------                       |
     |                          Err(Can't cast to list)                Ok(lv cons rv)                                |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Concat : BinaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                     *** eval(l: LetExpression, r: LetExpression, env: LetEnv) calculation ***                 |
     +===============================================================================================================+
     |                                                                                                               |
     |                                            lv = l.eval(env) ?!                                                |
     |                     ---------------------------------------------------------------------                     |
     |                                            rv = r.eval(env) ?!                                                |
     |                      -------------------------------------------------------------------                      |
     |                        lv !is number || rv !is number     lv is number && rv is number                        |
     |                       --------------------------------   ------------------------------                       |
     |                          Err(Can't cast to number)                Ok(lv + rv)                                 |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Add : BinaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                     *** eval(l: LetExpression, r: LetExpression, env: LetEnv) calculation ***                 |
     +===============================================================================================================+
     |                                                                                                               |
     |                                            lv = l.eval(env) ?!                                                |
     |                     ---------------------------------------------------------------------                     |
     |                                            rv = r.eval(env) ?!                                                |
     |                      -------------------------------------------------------------------                      |
     |                        lv !is number || rv !is number     lv is number && rv is number                        |
     |                       --------------------------------   ------------------------------                       |
     |                          Err(Can't cast to number)                Ok(lv > rv)                                 |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Greater : BinaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                     *** eval(l: LetExpression, r: LetExpression, env: LetEnv) calculation ***                 |
     +===============================================================================================================+
     |                                                                                                               |
     |                                            lv = l.eval(env) ?!                                                |
     |                     ---------------------------------------------------------------------                     |
     |                                            rv = r.eval(env) ?!                                                |
     |                      -------------------------------------------------------------------                      |
     |                        lv !is number || rv !is number     lv is number && rv is number                        |
     |                       --------------------------------   ------------------------------                       |
     |                          Err(Can't cast to number)                Ok(lv < rv)                                 |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Less : BinaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                     *** eval(l: LetExpression, r: LetExpression, env: LetEnv) calculation ***                 |
     +===============================================================================================================+
     |                                                                                                               |
     |                                            lv = l.eval(env) ?!                                                |
     |                     ---------------------------------------------------------------------                     |
     |                                            rv = r.eval(env) ?!                                                |
     |                      -------------------------------------------------------------------                      |
     |                        lv !is number || rv !is number     lv is number && rv is number                        |
     |                       --------------------------------   ------------------------------                       |
     |                          Err(Can't cast to number)                Ok(lv >= rv)                                 |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object GreaterEq : BinaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                     *** eval(l: LetExpression, r: LetExpression, env: LetEnv) calculation ***                 |
     +===============================================================================================================+
     |                                                                                                               |
     |                                            lv = l.eval(env) ?!                                                |
     |                     ---------------------------------------------------------------------                     |
     |                                            rv = r.eval(env) ?!                                                |
     |                      -------------------------------------------------------------------                      |
     |                        lv !is number || rv !is number     lv is number && rv is number                        |
     |                       --------------------------------   ------------------------------                       |
     |                          Err(Can't cast to number)                Ok(lv <= rv)                                 |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object LessEq : BinaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                     *** eval(l: LetExpression, r: LetExpression, env: LetEnv) calculation ***                 |
     +===============================================================================================================+
     |                                                                                                               |
     |                                            lv = l.eval(env) ?!                                                |
     |                     ---------------------------------------------------------------------                     |
     |                                            rv = r.eval(env) ?!                                                |
     |                      -------------------------------------------------------------------                      |
     |                        lv !is number || rv !is number     lv is number && rv is number                        |
     |                       --------------------------------   ------------------------------                       |
     |                          Err(Can't cast to number)                Ok(lv * rv)                                 |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Multiply : BinaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                     *** eval(l: LetExpression, r: LetExpression, env: LetEnv) calculation ***                 |
     +===============================================================================================================+
     |                                                                                                               |
     |                                            lv = l.eval(env) ?!                                                |
     |                     ---------------------------------------------------------------------                     |
     |                                            rv = r.eval(env) ?!                                                |
     |                      -------------------------------------------------------------------                      |
     |                        lv !is number || rv !is number     lv is number && rv is number                        |
     |                       --------------------------------   ------------------------------                       |
     |                          Err(Can't cast to number)                Ok(lv / rv)                                 |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Division : BinaryOperator()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                     *** eval(l: LetExpression, r: LetExpression, env: LetEnv) calculation ***                 |
     +===============================================================================================================+
     |                                                                                                               |
     |                                            lv = l.eval(env) ?!                                                |
     |                     ---------------------------------------------------------------------                     |
     |                                            rv = r.eval(env) ?!                                                |
     |                      -------------------------------------------------------------------                      |
     |                        lv !is number || rv !is number     lv is number && rv is number                        |
     |                       --------------------------------   ------------------------------                       |
     |                          Err(Can't cast to number)                Ok(lv % rv)                                 |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data object Reminder : BinaryOperator()
}

/*
     +---------------------------------------------------------------------------------------------------------------+
     |                                        *** eval(env: LetEnv) calculation ***                                  |
     +===============================================================================================================+
     |                                                                                                               |
     |                  Just(name to value) = this              Nested(name to value, rest) = this                   |
     |                -------------------------------         ---------------------------------------                |
     |                    v = value.eval(env) ?!                        v = value.eval(env) ?!                       |
     |                  ---------------------------             -----------------------------------                  |
     |                   Ok(env.append(name to v))                     env' = rest.eval(env) ?!                      |
     |                                                            -------------------------------                    |
     |                                                              Ok(env'.append(name to v))                       |
     |                                                                                                               |                         |
     +---------------------------------------------------------------------------------------------------------------+
*/
sealed class LetEnvBindingList<TVar> {
    class None<TVar> : LetEnvBindingList<TVar>()
    data class Nested<TVar>(
        val headBinding: Pair<TVar, LetExpression<TVar>>,
        val rest: LetEnvBindingList<TVar>
    ) : LetEnvBindingList<TVar>()
}

sealed class LetCondMatchList<TVar> {
    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                                        *** eval(env: LetEnv) calculation ***                                  |
     +===============================================================================================================+
     |                                                                                                               |
     |                                       Err(No match found in cond expr)                                        |
     |                                                                                                               |                         |
     +---------------------------------------------------------------------------------------------------------------+
    */
    class None<TVar> : LetCondMatchList<TVar>()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                                        *** eval(env: LetEnv) calculation ***                                  |
     +===============================================================================================================+
     |                                                                                                               |
     |                                                elseExpr.eval(env)                                             |
     |                                                                                                               |                         |
     +---------------------------------------------------------------------------------------------------------------+
    */
    data class Else<TVar>(val elseExpr: LetExpression<TVar>) : LetCondMatchList<TVar>()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                                        *** eval(env: LetEnv) calculation ***                                  |
     +===============================================================================================================+
     |                                                                                                               |
     |                              condValue = headMatch.first.eval(env) ?!                                         |
     |          -------------------------------------------------------------------------------------------          |
     |               condValue! is boolean      condValue is boolean(true)    condValue is boolean(false)            |
     |           ----------------------------  ----------------------------  -----------------------------           |
     |            Err(Can't cast to boolean)    headMatch.second.eval(env)          rest.eval(env)                   |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
    */
    data class Nested<TVar>(
        val headMatch: Pair<LetExpression<TVar>, LetExpression<TVar>>,
        val rest: LetCondMatchList<TVar>
    ) : LetCondMatchList<TVar>()
}

sealed class LetExpression<TVar> {
    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                                   *** eval(env: LetEnv) calculation ***                                       |
     +===============================================================================================================+
     |                                                                                                               |
     |                                                 Ok(value)                                                     |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    class EmptyList<TVar> : LetExpression<TVar>()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                                   *** eval(env: LetEnv) calculation ***                                       |
     +===============================================================================================================+
     |                                                                                                               |
     |                                                 Ok(value)                                                     |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data class Constant<TVar>(val value: LetValue) : LetExpression<TVar>()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                                   *** eval(env: LetEnv) calculation ***                                       |
     +===============================================================================================================+
     |                                                                                                               |
     |                                          cond = condition.eval(env) ?!                                        |
     |             --------------------------------------------------------------------------------------            |
     |                        cond is number                           cond is bool                                  |
     |               -------------------------------   ------------------------------------------------              |
     |                    Err(Can't cast to bool)            cond is true          cond is false                     |
     |                                                    -------------------  ----------------------                |
     |                                                       then.eval(env)      otherwise.eval(env)                 |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data class If<TVar>(
        val condition: LetExpression<TVar>,
        val then: LetExpression<TVar>,
        val otherwise: LetExpression<TVar>
    ) : LetExpression<TVar>()

    data class Cond<TVar>(val matches: LetCondMatchList<TVar>) : LetExpression<TVar>()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                                   *** eval(env: LetEnv) calculation ***                                       |
     +===============================================================================================================+
     |                                                                                                               |
     |                                           value = expr.eval(env) ?!                                           |
     |                                         -----------------------------                                         |
     |                                               value.print()                                                   |
     |                                            -------------------                                                |
     |                                                   Ok(1)                                                       |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data class Print<TVar>(val expr: LetExpression<TVar>) : LetExpression<TVar>()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                                   *** eval(env: LetEnv) calculation ***                                       |
     +===============================================================================================================+
     |                                                                                                               |
     |                                           operator.eval(l, r, env)                                            |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data class Binary<TVar>(
        val operator: BinaryOperator,
        val l: LetExpression<TVar>,
        val r: LetExpression<TVar>
    ) : LetExpression<TVar>()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                                   *** eval(env: LetEnv) calculation ***                                       |
     +===============================================================================================================+
     |                                                                                                               |
     |                                        operator.eval(operand, env)                                            |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data class Unary<TVar>(
        val operator: UnaryOperator,
        val operand: LetExpression<TVar>
    ) : LetExpression<TVar>()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                                        *** eval(env) calculation ***                                          |
     +===============================================================================================================+
     |                                                                                                               |
     |                                        boundVar = env.get(boundName)                                          |
     |                --------------------------------------------------------------------------                     |
     |                             None = boundVar                 Some(varValue) = boundVar                         |
     |                  ---------------------------------------  -----------------------------                       |
                          Err(Variable 'boundName' not found)            Ok(varValue)                                |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data class Var<TVar>(val boundName: TVar) : LetExpression<TVar>()

    /*
     +---------------------------------------------------------------------------------------------------------------+
     |                                         *** eval(env) calculation ***                                         |
     +===============================================================================================================+
     |                                                                                                               |
     |                                          env' = bindings.eval(env) ?!                                         |
     |                                        --------------------------------                                       |
     |                                               expr.eval(env')                                                 |
     |                                                                                                               |
     +---------------------------------------------------------------------------------------------------------------+
     */
    data class Let<TVar>(
        val bindings: LetEnvBindingList<TVar>,
        val expr: LetExpression<TVar>
    ) : LetExpression<TVar>()
}

fun <TVar, TEnv: LetEnv<TVar, TEnv>> LetEnvBindingList<TVar>.eval(env: TEnv): Result<TEnv, LetError<TVar>> =
    when(this) {
        is LetEnvBindingList.None -> ok { env }
        is LetEnvBindingList.Nested -> rest.eval(env).flatMap { newEnv ->
            headBinding.second.eval(env).flatMap { v -> ok { newEnv.append(headBinding.first to v) } }
        }
    }

fun <TVar, TEnv: LetEnv<TVar, TEnv>> LetCondMatchList<TVar>.eval(env: TEnv): Result<LetValue, LetError<TVar>> =
    when(this) {
        is LetCondMatchList.Nested -> headMatch.first.eval(env).flatMap { condValue ->
            when (condValue) {
                is LetValue.Boolean -> if (condValue.value) headMatch.second.eval(env) else rest.eval(env)
                else -> err { LetError.CantCastToBool() }
            }
        }
        is LetCondMatchList.Else -> elseExpr.eval(env)
        is LetCondMatchList.None -> err { LetError.NoSuitableCondMatchFound() }
    }

fun <TVar, TEnv: LetEnv<TVar, TEnv>> UnaryOperator.eval(
    operand: LetExpression<TVar>,
    env: TEnv
): Result<LetValue, LetError<TVar>> =
    operand.eval(env).flatMap { oper ->
        when (this) {
            UnaryOperator.Empty -> when {
                oper is LetValue.List -> { ok { LetValue.from(oper is LetValue.List.Empty) } }
                else -> err { LetError.CantCastToList() }
            }
            UnaryOperator.Length -> when {
                oper is LetValue.List -> { ok { LetValue.from(oper.length) } }
                else -> err { LetError.CantCastToList() }
            }
            UnaryOperator.Car -> when (oper) {
                is LetValue.List.Cons -> { ok { oper.head } }
                is LetValue.List.Empty -> err { LetError.CantCarFromEmptyList() }
                else -> err { LetError.CantCastToList() }
            }
            UnaryOperator.Cdr -> when (oper) {
                is LetValue.List.Cons -> { ok { oper.tail } }
                is LetValue.List.Empty -> err { LetError.CantCarFromEmptyList() }
                else -> err { LetError.CantCastToList() }
            }
            UnaryOperator.Minus -> when {
                oper is LetValue.Number -> ok { LetValue.from(-oper.value) }
                else -> err { LetError.CantCastToNumber() }
            }
            UnaryOperator.Not -> when {
                oper is LetValue.Boolean -> ok { LetValue.from(!oper.value) }
                else -> err { LetError.CantCastToBool() }
            }
            UnaryOperator.IsZero -> when {
                oper is LetValue.Number -> ok { LetValue.from(oper.value == 0) }
                else -> err { LetError.CantCastToNumber() }
            }
            UnaryOperator.AsBool ->
                when (oper) {
                    is LetValue.Boolean -> ok { oper }
                    is LetValue.Number -> ok {LetValue.from(oper.value != 0) }
                    else -> err { LetError.CantCastToBool() }
                }

            UnaryOperator.AsNumber ->
                when (oper) {
                    is LetValue.Number -> ok { oper }
                    is LetValue.Boolean -> ok { LetValue.from( if (oper.value) 1 else 0) }
                    else -> err { LetError.CantCastToNumber() }
                }

        }
    }

val BinaryOperator.isArithmetic get() =
    this is BinaryOperator.Add ||
    this is BinaryOperator.Subtract ||
    this is BinaryOperator.Multiply ||
    this is BinaryOperator.Division ||
    this is BinaryOperator.Reminder

val BinaryOperator.isListOperator get() =
    this is BinaryOperator.Cons ||
    this is BinaryOperator.Concat

val BinaryOperator.isComparison get() =
    this is BinaryOperator.Greater ||
    this is BinaryOperator.GreaterEq ||
    this is BinaryOperator.Less ||
    this is BinaryOperator.LessEq

val BinaryOperator.isBoolean get() =
    this is BinaryOperator.And ||
    this is BinaryOperator.Or ||
    this is BinaryOperator.Xor

fun <TVar, TEnv: LetEnv<TVar, TEnv>> BinaryOperator.eval(
    l: LetExpression<TVar>,
    r: LetExpression<TVar>,
    env: TEnv
): Result<LetValue, LetError<TVar>> =
    l.eval(env).flatMap { lhs ->
        r.eval(env).flatMap { rhs ->
            when {
                isArithmetic -> {
                    when {
                        lhs is LetValue.Number && rhs is LetValue.Number ->
                            when (this) {
                                BinaryOperator.Add -> ok { LetValue.from(lhs.value + rhs.value) }
                                BinaryOperator.Division -> ok { LetValue.from(lhs.value / rhs.value) }
                                BinaryOperator.Multiply -> ok { LetValue.from(lhs.value * rhs.value) }
                                BinaryOperator.Reminder -> ok { LetValue.from(lhs.value % rhs.value) }
                                BinaryOperator.Subtract -> ok { LetValue.from(lhs.value - rhs.value) }
                                else -> err { LetError.UnsupportedArithmeticOperator(this) }
                            }

                        else -> err { LetError.CantCastToNumber() }
                    }
                }
                isListOperator -> {
                    when {
                        rhs is LetValue.List ->
                            when (this) {
                                BinaryOperator.Cons -> ok { lhs cons rhs }
                                BinaryOperator.Concat ->
                                    if (lhs is LetValue.List) {
                                        ok { lhs concat rhs }
                                    } else {
                                        err { LetError.CantCastToList() }
                                    }

                                else -> err { LetError.UnsupportedListOperator(this) }
                            }

                        else -> err { LetError.CantCastToList() }
                    }
                }
                isBoolean -> {
                    when {
                        lhs is LetValue.Boolean && rhs is LetValue.Boolean ->
                            when (this) {
                                BinaryOperator.And -> ok { LetValue.from(lhs.value && rhs.value) }
                                BinaryOperator.Or -> ok { LetValue.from(lhs.value && rhs.value) }
                                BinaryOperator.Xor -> ok { LetValue.from(lhs.value && rhs.value) }
                                else -> err { LetError.UnsupportedBooleanOperator(this) }
                            }
                        else -> err { LetError.CantCastToBool() }
                    }
                }
                isComparison -> {
                    when {
                        lhs is LetValue.Number && rhs is LetValue.Number ->
                            when (this) {
                                BinaryOperator.Greater -> ok { LetValue.from(lhs.value > rhs.value) }
                                BinaryOperator.GreaterEq -> ok { LetValue.from(lhs.value >= rhs.value) }
                                BinaryOperator.Less -> ok { LetValue.from(lhs.value < rhs.value) }
                                BinaryOperator.LessEq -> ok { LetValue.from(lhs.value <= rhs.value) }
                                else -> err { LetError.UnsupportedComparisonOperator(this) }
                            }
                        else -> err { LetError.CantCompareTypes(lhs, rhs) }
                    }
                }
                this is BinaryOperator.Eq -> lhs.structuralEq(rhs)
                else -> {
                    err { LetError.UnsupportedOperator(this) }
                }
            }
        }
    }

fun <TVar, TEnv: LetEnv<TVar, TEnv>> LetExpression<TVar>.eval(env: TEnv): Result<LetValue, LetError<TVar>> =
    when (this) {
        is LetExpression.EmptyList -> ok { LetValue.List.Empty }
        is LetExpression.Constant -> ok { value }
        is LetExpression.Print -> expr.eval(env).flatMap { ok { println(it.printed()); it } }
        is LetExpression.Binary -> operator.eval(l, r, env)
        is LetExpression.Unary -> operator.eval(operand, env)
        is LetExpression.If ->
            condition.eval(env).flatMap { cond ->
                cond
                    .tryCast<LetValue.Boolean>()
                    .okOr<LetValue.Boolean, LetError<TVar>> { err { LetError.CantCastToBool() } }
                    .flatMap { boolCond -> if (boolCond.value) then.eval(env) else otherwise.eval(env) }
            }
        is LetExpression.Cond -> matches.eval(env)
        is LetExpression.Let -> {
            bindings.eval(env).flatMap { newEnv -> expr.eval(newEnv) }
        }

        is LetExpression.Var ->
            env.get(boundName).okOr { err { LetError.VarNotFound(boundName) } }
    }

data class LetProgram<TVar>(val expr: LetExpression<TVar>)
fun <TVar, TEnv: LetEnv<TVar, TEnv>> LetProgram<TVar>.eval(env: TEnv): Result<LetValue, LetError<TVar>> =
    expr.eval(env)

object StringLetProgramGrammar : Grammar<LetProgram<String>>() {
    // scanning
    @Suppress("unused")
    private val _ws by regexToken("\\s+", ignore = true)

    private val letKw by literalToken("let")
    private val inKw by literalToken("in")
    private val ifKw by literalToken("if")
    private val condKw by literalToken("cond")
    private val thenKw by literalToken("then")
    private val elseKw by literalToken("else")
    private val endKw by literalToken("end")
    private val notKw by literalToken("not")
    private val minusKw by literalToken("minus")
    private val zeroCheckKw by literalToken("zero?")
    private val emptyCheckKw by literalToken("empty?")
    private val lengthKw by literalToken("length")
    private val numberKw by literalToken("number")
    private val boolKw by literalToken("bool")
    private val consKw by literalToken("cons")
    private val concatKw by literalToken("concat")
    private val carKw by literalToken("car")
    private val cdrKw by literalToken("cdr")
    private val eqCheckKw by literalToken("eq?")
    private val greaterKw by literalToken("greater?")
    private val lesserKw by literalToken("lesser?")
    private val greaterEqKw by literalToken("greater-eq?")
    private val lesserEqKw by literalToken("lesser-eq?")
    private val printKw by literalToken("print!")
    private val arrowSign by literalToken("->")
    private val eqSign by literalToken("=")
    private val minusOp by literalToken("-")
    private val plusOp by literalToken("+")
    private val mulOp by literalToken("*")
    private val divOp by literalToken("/")
    private val remOp by literalToken("%")
    private val comma by literalToken(",")
    private val andOp by literalToken("and")
    private val orOp by literalToken("or")
    private val xorOp by literalToken("xor")
    private val lParen by literalToken("(")
    private val rParen by literalToken(")")
    private val lSqParen by literalToken("[")
    private val rSqParen by literalToken("]")
    private val id by regexToken("([:_]|[a-zA-Z])([a-zA-Z]|[0-9]|[:_\\-!?])*")
    private val trueLit by literalToken("#t")
    private val falseLit by literalToken("#f")
    private val number by regexToken("[0-9]+")

    //parsing
    private val parseVarExpr: Parser<LetExpression<String>> by id
        .use { LetExpression.Var(text) }

    private val parseEmptyListExpr: Parser<LetExpression<String>> by (lSqParen * -rSqParen)
        .use { LetExpression.EmptyList() }

    private val parseNumberConstantExpr: Parser<LetExpression<String>> by number
        .use { LetExpression.Constant(LetValue.from(text.toInt())) }

    private val parseTrueLiteralExpr: Parser<LetExpression<String>> by trueLit
        .use { LetExpression.Constant(LetValue.from(true)) }

    private val parseFalseLiteralExpr: Parser<LetExpression<String>> by falseLit
        .use { LetExpression.Constant(LetValue.from(false)) }

    private val parseCondExpr : Parser<LetExpression<String>> by
        (
            -condKw *
            oneOrMore(parser { parseExpr } * -arrowSign * parser { parseExpr }) *
            optional( -elseKw * -arrowSign * parser { parseExpr }) *
            -endKw
        ).use {
            val matches = t1
            val elseClause = t2
            val tail = if (elseClause != null) LetCondMatchList.Else(elseClause) else LetCondMatchList.None()
            LetExpression.Cond(
                matches
                    .reversed()
                    .fold(tail) { acc, next -> LetCondMatchList.Nested(next.t1 to next.t2, acc) }
            )
        }

    private val parseIfExpr: Parser<LetExpression<String>> by
        (-ifKw * parser { parseExpr } * -thenKw * parser { parseExpr } * -elseKw * parser { parseExpr })
            .use { LetExpression.If(t1, t2, t3) }

    private val parseListLiteral: Parser<LetExpression<String>> by
        (
            -lSqParen *
            parser { parseExpr } *
            zeroOrMore(-comma * parser { parseExpr }) *
            -rSqParen
        ).use {
            LetExpression.Binary(
                BinaryOperator.Cons,
                t1,
                t2.reversed()
                    .fold(LetExpression.EmptyList<String>() as LetExpression<String>) { acc, next ->
                        LetExpression.Binary(BinaryOperator.Cons, next, acc)
                    }
            )
        }

    private val parseLetExpr: Parser<LetExpression<String>> by
        (
            -letKw *
            -lSqParen *
            id *
            -eqSign *
            parser { parseExpr } *
            zeroOrMore(-comma * id * -eqSign * parser { parseExpr }) *
            -rSqParen *
            -inKw *
            parser { parseExpr }
        ).use {
            LetExpression.Let(
                LetEnvBindingList.Nested(
                    t1.text to t2,
                    t3.reversed().fold(LetEnvBindingList.None<String>() as LetEnvBindingList<String>) {
                            acc, next ->
                        LetEnvBindingList.Nested(
                            next.t1.text to next.t2,
                            acc
                        )
                    }
                ),
                t4
            )
        }

    private val parseUnaryOperator by
        notKw.use { UnaryOperator.Not } or
        minusKw.use { UnaryOperator.Minus } or
        zeroCheckKw.use { UnaryOperator.IsZero } or
        carKw.use { UnaryOperator.Car } or
        cdrKw.use { UnaryOperator.Cdr } or
        emptyCheckKw.use { UnaryOperator.Empty } or
        lengthKw.use { UnaryOperator.Length } or
        numberKw.use { UnaryOperator.AsNumber } or
        boolKw.use { UnaryOperator.AsBool }

    private val parseUnaryExpr: Parser<LetExpression<String>> by
        (parseUnaryOperator * -lParen * parser { parseExpr } * -rParen)
            .use { LetExpression.Unary(t1, t2) }

    private val parsePrintExpr: Parser<LetExpression<String>> by
        (-printKw * -lParen * parser { parseExpr } * -rParen)
            .use { LetExpression.Print(this) }

    private val parseBinaryOperator by
        minusOp.use { BinaryOperator.Subtract } or
        plusOp.use { BinaryOperator.Add } or
        divOp.use { BinaryOperator.Division } or
        mulOp.use { BinaryOperator.Multiply } or
        remOp.use { BinaryOperator.Reminder } or
        consKw.use { BinaryOperator.Cons } or
        concatKw.use { BinaryOperator.Concat } or
        andOp.use { BinaryOperator.And } or
        orOp.use { BinaryOperator.Or } or
        xorOp.use { BinaryOperator.Xor } or
        eqCheckKw.use { BinaryOperator.Eq } or
        lesserKw.use { BinaryOperator.Less } or
        lesserEqKw.use { BinaryOperator.LessEq } or
        greaterKw.use { BinaryOperator.Greater } or
        greaterEqKw.use { BinaryOperator.GreaterEq }

    private val parseBinaryExpr: Parser<LetExpression<String>> by
        (parseBinaryOperator * -lParen * parser { parseExpr } * -comma * parser { parseExpr } * -rParen)
            .use { LetExpression.Binary(t1, t2, t3) }

    private val parseExpr by
        parseIfExpr or
        parseCondExpr or
        parseEmptyListExpr or
        parseVarExpr or
        parseNumberConstantExpr or
        parseTrueLiteralExpr or
        parseFalseLiteralExpr or
        parseLetExpr or
        parseBinaryExpr or
        parseUnaryExpr or
        parseListLiteral or
        parsePrintExpr

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

        val result = parsedAst.eval(env)
        println(result)
        println()
        return result
    }

    test("a")
    test("zero?(a)")
    test("bool(a)")
    test("minus(a)")
    test("b")
    test("zero?(b)")
    test("bool(b)")
    test("minus(b)")
    test("c")
    test("zero?(c)")
    test("bool(c)")
    test("not(c)")
    test("d")
    test("zero?(d)")
    test("minus(d)")
    test("bool(d)")
    test("e")
    test("zero?(e)")
    test("number(e)")
    test("if c then 1 else 0")
    test("-(a, b)")
    test("+(a, b)")
    test("*(a, b)")
    test("/(a, b)")
    test("%(a, b)")
    test("let [e = if c then 5 else 10] in -(+(a, e), b)")
    test("let [e = #t] in if e then 100500 else #f")
    test(
        """
            zero?(
                let [z = 2, x = 3] in 
                let [y = -(x, 1)] in
                let [x = 4] in -(z, -(x, y)))
        """.trimIndent()
    )
    test(
        """
            let [x = 2, y = 3] in
            let [x = -(y, x)] in
            -(y, x)
        """.trimIndent()
    )
    test(
        """
            let [l1 = cons(1, [2, 3]), l2 = cons(5, cons(6, []))] in
            +(car(cdr(l1)), car(l2))
        """.trimIndent()
    )
    test(
        """
            let [l1 = [1, 2, 3], l2 = [5, 6]] in
            +(car(cdr(cdr(l1))) , car(l2))
        """.trimIndent()
    )
    test(
        """
            let [l1 = print!([1, 2, 3]), l2 = print!([4, 5])] in
            let [l = print!(concat(l1, l2))] in
            print!([eq?(length(l), 3), eq?(length(l1), 3), eq?(length(l2), 3), eq?(l, [1, 2, 3, 4, 5])])
        """.trimIndent()
    )
    test(
        """
            let [l = [1, #t, [1, 2, 3], [#f, #f, #f]]] in
            print!(l)
        """.trimIndent()
    )
    test(
        """
            let [x = 3, y = 5, z = #t] in
            cond
                eq?(x, 5) -> x
                eq?(y, 3) -> y
                not(z) -> z
            end
        """.trimIndent()
    )
    test(
        """
            let [x = 3, y = 5, z = #t] in
            cond
                eq?(x, 5) -> x
                eq?(y, 3) -> y
                not(z) -> z
                else -> 100500
            end
        """.trimIndent()
    )
    test(
        """
            let [x = 3, y = 5, z = #t] in
            cond
                eq?(x, 5) -> x
                eq?(y, 5) -> y
                not(z) -> z
                else -> 100500
            end
        """.trimIndent()
    )
}
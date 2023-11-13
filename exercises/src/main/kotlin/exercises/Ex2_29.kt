package exercises

sealed class SExpr {
    data object Nil : SExpr()
    data class Number(val data: Double) : SExpr()
    data class Char(val data: kotlin.Char) : SExpr()
    data class String(val data: kotlin.String) : SExpr()
    data class Identifier(val data: kotlin.String) : SExpr()

    sealed class List : SExpr() {
        data object Empty: List()
        data class Pair(val head: SExpr, val tail: SExpr) : List() {
            fun <T> fold(init: T, step: (T, SExpr) -> T): T = if (tail !is Pair) {
                step(init, head)
            } else {
                tail.fold(step(init, head), step)
            }
            fun forEach(step: (SExpr) -> Unit) = fold(Unit) { _, next -> step(next) }
        }

        companion object {
            fun of(vararg entries: SExpr): List = when {
                entries.isEmpty() -> Empty
                else -> entries.indices
                    .reversed()
                    .fold(Nil as SExpr) { acc, i -> Pair(entries[i], acc) } as List
            }
        }
    }

    fun toPrintableString(): kotlin.String {
        return when (this) {
            is Char -> "'${this.data}'"
            is Identifier -> this.data
            List.Empty -> "( )"
            is List.Pair -> "( ${this.fold("") { acc, next -> acc + next.toPrintableString() + " " }})"
            Nil -> "nil"
            is Number -> "${this.data}"
            is String -> "\"${this.data}\""
        }
    }

    companion object {
        fun parse(source: kotlin.String): SExpr {
            TODO()
        }
        fun lambda(boundVars: List, body: SExpr): List = List.of(Identifier("lambda"), boundVars, body)
        fun apply(rator: SExpr, vararg rands: SExpr) = SExpr.List.Pair(
            rator,
            when {
                rands.isEmpty() -> List.Empty
                else -> rands.indices
                    .reversed()
                    .fold(Nil as SExpr) { acc, i -> List.Pair(rands[i], acc) }
            }
        )
    }
}

sealed class LExpr {
    data class Identifier(val data: String) : LExpr()
    data class Application(val rator: LExpr, val rands: List<LExpr>) : LExpr()
    data class Lambda(val boundVars: List<LExpr>, val body: LExpr) : LExpr()
    class ParseException(errorMessage: String) : Exception(errorMessage)

    fun unparse(): SExpr {
        return when (this) {
            is Application -> SExpr.List.Pair(
                rator.unparse(),
                rands.fold(SExpr.Nil as SExpr) { acc, next -> SExpr.List.Pair(next.unparse(), acc) }
            )
            is Identifier -> SExpr.Identifier(this.data)
            is Lambda -> {
                SExpr.lambda(
                    if (boundVars.isEmpty()) {
                        SExpr.List.Empty
                    } else {
                        boundVars.fold(SExpr.Nil as SExpr) { acc, next -> SExpr.List.Pair(next.unparse(), acc) } as SExpr.List
                    },
                    when (body) {
                        is Application -> SExpr.List.Pair(
                            body.rator.unparse(),
                            body.rands.fold(SExpr.Nil as SExpr) { acc, next -> SExpr.List.Pair(next.unparse(), acc) }
                        )
                        is Identifier -> body.unparse()
                        is Lambda -> body.unparse()
                    }
                )
            }
        }
    }

    companion object {
        private fun parseApplication(datum: SExpr.List.Pair): Application {
            val rator = parse(datum.head)
            val rands = mutableListOf<LExpr>()
            val randsList = datum.tail as? SExpr.List.Pair
                ?: throw ParseException("rands should be pair!")
            randsList.forEach { rands.add(parse(it)) }
            rands.reverse()

            return Application(rator, rands)
        }

        private fun parseLambda(datum: SExpr): Lambda {
            val (bVars, rest) = datum as? SExpr.List.Pair
                ?: throw ParseException("expected pair, found empty list!")

            val boundVarList = bVars as? SExpr.List.Pair
                ?: throw ParseException("bound vars should be Pair!")

            val boundVars = mutableListOf<LExpr>()
            boundVarList.forEach {
                val boundVarIdent = it as? SExpr.Identifier
                    ?: throw ParseException("expected identifier!")
                boundVars.add(parseIdentifier(boundVarIdent))
            }
            boundVars.reverse()

            val bodyList = rest as? SExpr.List.Pair
                ?: throw ParseException("expected pair, found empty list!")

            val body = parse(bodyList.head)

            if (bodyList.tail !is SExpr.Nil) {
                throw ParseException("found garbage in tail of lambda expr")
            }

            return Lambda(boundVars, body)
        }

        private fun parseIdentifier(datum: SExpr.Identifier): Identifier {
            if (datum.data == "lambda") {
                throw ParseException("found \"lambda\" identifier in incorrect place!")
            }
            return Identifier(datum.data)
        }

        private fun parseApplicationOrLambda(datum: SExpr.List.Pair): LExpr = when {
            datum.head is SExpr.Identifier && datum.head.data == "lambda" -> parseLambda(datum.tail)
            else -> parseApplication(datum)
        }

        fun parse(datum: SExpr): LExpr = when(datum) {
            is SExpr.Identifier -> parseIdentifier(datum)
            is SExpr.List.Pair -> parseApplicationOrLambda(datum)
            is SExpr.Char -> throw ParseException("expected pair of ident, but found char!")
            SExpr.List.Empty -> throw ParseException("expected pair of ident, but found empty list!")
            SExpr.Nil -> throw ParseException("expected pair of ident, but found nil!")
            is SExpr.Number -> throw ParseException("expected pair of ident, but found number!")
            is SExpr.String -> throw ParseException("expected pair of ident, but found string!")
        }
    }
}

fun ex2_29() {
    fun testSExpr(sExpr: SExpr) {
        println(sExpr)
        println(sExpr.toPrintableString())
        val lExpr = LExpr.parse(sExpr)
        println(lExpr)
        val sExprUnParsed = lExpr.unparse()
        println(sExprUnParsed)
        println(sExprUnParsed.toPrintableString())
        println()
    }

    val testSexpr = SExpr.lambda(
        SExpr.List.of(
            SExpr.Identifier("a"),
            SExpr.Identifier("b"),
            SExpr.Identifier("c")
        ),
        SExpr.apply(
            SExpr.Identifier("add"),
            SExpr.Identifier("a"),
            SExpr.Identifier("b"),
            SExpr.Identifier("c")
        )
    )
    testSExpr(testSexpr)

    val smallSExpr = SExpr.apply(
        SExpr.lambda(
            SExpr.List.of(SExpr.Identifier("a")),
            SExpr.apply(SExpr.Identifier("a"), SExpr.Identifier("b"))
        ),
        SExpr.Identifier("c")
    )
    testSExpr(smallSExpr)

    val sExpr = SExpr.lambda(
        SExpr.List.of(SExpr.Identifier("x")),
        SExpr.lambda(
            SExpr.List.of(SExpr.Identifier("y")),
            SExpr.apply(
                SExpr.lambda(
                    SExpr.List.of(SExpr.Identifier("x")),
                    SExpr.apply(SExpr.Identifier("x"), SExpr.Identifier("y"))
                ),
                SExpr.Identifier("x")
            )
        )
    )
    testSExpr(sExpr)
}
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
            fun <T> fold(init: T, step: (T, SExpr) -> T): T {
                val v = step(init, head)
                return if (tail !is Pair) { v } else { tail.fold(v, step) }
            }
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
            is Char -> "'$data'"
            is Identifier -> data
            List.Empty -> "( )"
            is List.Pair -> "( ${fold("") { acc, next -> acc + next.toPrintableString() + " " }})"
            Nil -> "nil"
            is Number -> "$data"
            is String -> "\"$data\""
        }
    }

    companion object {
        @Suppress("unused")
        fun parse(source: kotlin.String): SExpr {
            println(source)
            TODO()
        }
        fun lambda(boundVars: List, body: SExpr): List = List.of(Identifier("lambda"), boundVars, body)
        fun apply(rator: SExpr, rands: List) = List.Pair(rator, rands)
    }
}

sealed class LExpr {
    data class Identifier(val data: String) : LExpr()
    data class Application(val rator: LExpr, val rands: List<LExpr>) : LExpr()
    data class Lambda(val boundVars: List<LExpr>, val body: LExpr) : LExpr()
    class ParseException(errorMessage: String) : Exception(errorMessage)

    fun unParse(): SExpr {
        fun List<LExpr>.toSExpr() : SExpr.List = when {
            this.isEmpty() -> SExpr.List.Empty
            else -> reversed().fold(SExpr.Nil as SExpr) { acc, next -> SExpr.List.Pair(next.unParse(), acc) } as SExpr.List
        }

        return when (this) {
            is Application -> SExpr.apply(rator.unParse(), rands.toSExpr())
            is Identifier -> SExpr.Identifier(data)
            is Lambda -> SExpr.lambda(boundVars.toSExpr(), body.unParse())
        }
    }

    companion object {
        private fun parseApplication(datum: SExpr.List.Pair): Application {
            val randsList = datum.tail as? SExpr.List.Pair
                ?: throw ParseException("rands should be pair!")

            return Application(
                parse(datum.head),
                randsList.fold(mutableListOf()) { acc, next ->
                    acc.add(parse(next))
                    acc
                }
            )
        }

        private fun parseLambda(datum: SExpr): Lambda {
            val (bVars, rest) = datum as? SExpr.List.Pair
                ?: throw ParseException("expected pair, found empty list!")

            val boundVarList = bVars as? SExpr.List.Pair
                ?: throw ParseException("bound vars should be Pair!")

            val bodyList = rest as? SExpr.List.Pair
                ?: throw ParseException("expected pair, found empty list!")

            if (bodyList.tail !is SExpr.Nil) {
                throw ParseException("found garbage in tail of lambda expr")
            }

            return Lambda(
                boundVarList.fold(mutableListOf()) { acc, next ->
                    val boundVarIdent = next as? SExpr.Identifier
                        ?: throw ParseException("expected identifier!")
                    acc.add(parseIdentifier(boundVarIdent))
                    acc
                },
                parse(bodyList.head)
            )
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
        val sExprUnParsed = lExpr.unParse()
        println(sExprUnParsed)
        println(sExprUnParsed.toPrintableString())
        println()
    }

    val testSExpr = SExpr.lambda(
        SExpr.List.of(
            SExpr.Identifier("a"),
            SExpr.Identifier("b"),
            SExpr.Identifier("c")
        ),
        SExpr.apply(
            SExpr.Identifier("add"),
            SExpr.List.of(
                SExpr.Identifier("a"),
                SExpr.Identifier("b"),
                SExpr.Identifier("c")
            )
        )
    )
    testSExpr(testSExpr)

    val smallSExpr = SExpr.apply(
        SExpr.lambda(
            SExpr.List.of(SExpr.Identifier("a")),
            SExpr.apply(
                SExpr.Identifier("a"),
                SExpr.List.of(SExpr.Identifier("b"))
            )
        ),
        SExpr.List.of(SExpr.Identifier("c"))
    )
    testSExpr(smallSExpr)

    val sExpr = SExpr.lambda(
        SExpr.List.of(SExpr.Identifier("x")),
        SExpr.lambda(
            SExpr.List.of(SExpr.Identifier("y")),
            SExpr.apply(
                SExpr.lambda(
                    SExpr.List.of(SExpr.Identifier("x")),
                    SExpr.apply(
                        SExpr.Identifier("x"),
                        SExpr.List.of(SExpr.Identifier("y"))
                    )
                ),
                SExpr.List.of(SExpr.Identifier("x"))
            )
        )
    )
    testSExpr(sExpr)
}
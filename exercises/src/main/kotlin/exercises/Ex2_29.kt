package exercises

import java.util.*

sealed class SExpr {
    data object Nil : SExpr()
    data class Identifier(val data: String) : SExpr()
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
                else -> entries.indices.reversed().fold(Nil as SExpr) { a, i -> Pair(entries[i], a) } as List
            }
        }
    }

    private sealed class Token {
        data object Whitespace: Token()
        data object LParen: Token()
        data object RParen: Token()
        data class Identifier(val data: String): Token()
    }

    fun toPrintableString(): String {
        return when (this) {
            is Identifier -> data
            List.Empty -> "( )"
            is List.Pair -> "( ${fold("") { a, n -> a + n.toPrintableString() + " " }})"
            Nil -> "nil"
        }
    }

    companion object {
        private fun scan(source: String): kotlin.collections.List<Token> {
            fun CharIterator.tryNext(): Char? = if (!hasNext()) { null } else { nextChar() }

            val charIterator = source.iterator()
            var eatenChar: Char? = null
            val stream = mutableListOf<Token>()
            var complete = false

            while (!complete) {
                val next = if (eatenChar != null) {
                    val c = eatenChar
                    eatenChar = null
                    c
                } else {
                    charIterator.tryNext()
                }

                if (next == null) { break }

                val simpleToken = when (next) {
                    '(' -> Token.LParen
                    ')' -> Token.RParen
                    else -> null
                }

                if (simpleToken != null) {
                    stream.add(simpleToken)
                } else if (next.isWhitespace()) {
                    while (true) {
                        when(val c = charIterator.tryNext()) {
                            null -> {
                                stream.add(Token.Whitespace)
                                complete = true
                                break
                            }
                            else -> {
                                if (!c.isWhitespace()) {
                                    eatenChar = c
                                    stream.add(Token.Whitespace)
                                    break
                                }
                            }
                        }
                    }
                } else if (next.isLetter()) {
                    var tokenData = String(charArrayOf(next))
                    while (true) {
                        when(val c = charIterator.tryNext()) {
                            null -> {
                                stream.add(Token.Identifier(tokenData))
                                complete = true
                                break
                            }
                            else -> {
                                if (!c.isLetter()) {
                                    eatenChar = c
                                    stream.add(Token.Identifier(tokenData))
                                    break
                                } else {
                                    tokenData += c
                                }
                            }
                        }
                    }
                } else {
                    throw Exception("found a bad character")
                }
            }
            return stream
        }

        fun parse(source: String): SExpr {
            val tokens = scan(source)
            var contents = mutableListOf<SExpr>()
            val groupStack: Stack<MutableList<SExpr>> = Stack()
            var justStarted = true

            var result: SExpr? = null

            for (token in tokens) {
                if (result != null) {
                    throw Exception("Found tokens after a closing paren")
                }

                if (justStarted) {
                    if (token != Token.LParen) {
                        throw Exception("S-expr is expected to be opened by '('")
                    }
                    justStarted = false
                    continue
                }

                when (token) {
                    Token.Whitespace -> { continue }
                    is Token.Identifier -> {
                        contents.add(
                            if (token.data == "nil") {
                                Nil
                            } else {
                                Identifier(token.data)
                            }
                        )
                    }
                    Token.LParen -> {
                        groupStack.push(contents)
                        contents = mutableListOf()
                    }
                    Token.RParen -> {
                        val resultContent = if (contents.isEmpty()) {
                            List.Empty
                        } else {
                            contents.reversed().fold(Nil as SExpr) { a, n -> List.Pair(n, a) }
                        }

                        if (groupStack.isEmpty()) {
                            result = resultContent
                        } else {
                            contents = groupStack.pop()
                            contents.add(resultContent)
                        }
                    }
                }
            }

            if (result == null) {
                throw Exception("S-expr is expected to be opened by ')'")
            }

            return result
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
            else -> reversed().fold(SExpr.Nil as SExpr) { a, n -> SExpr.List.Pair(n.unParse(), a) } as SExpr.List
        }

        return when (this) {
            is Application -> SExpr.apply(rator.unParse(), rands.toSExpr())
            is Identifier -> SExpr.Identifier(data)
            is Lambda -> SExpr.lambda(boundVars.toSExpr(), body.unParse())
        }
    }

    companion object {
        private fun parseApplication(datum: SExpr.List.Pair): Application {
            val randsList = datum.tail as? SExpr.List.Pair ?: throw ParseException("rands should be pair!")

            return Application(
                parse(datum.head),
                randsList.fold(mutableListOf()) { a, n -> a.add(parse(n)); a }
            )
        }

        private fun parseLambda(datum: SExpr): Lambda {
            val (bVars, rest) = datum as? SExpr.List.Pair ?: throw ParseException("expected pair, found empty list!")

            val boundVarList = bVars as? SExpr.List.Pair ?: throw ParseException("bound vars should be Pair!")

            val bodyList = rest as? SExpr.List.Pair ?: throw ParseException("expected pair, found empty list!")

            if (bodyList.tail !is SExpr.Nil) { throw ParseException("found garbage in tail of lambda expr") }

            return Lambda(
                boundVarList.fold(mutableListOf()) { a, n ->
                    val boundVarIdent = n as? SExpr.Identifier ?: throw ParseException("expected identifier!")
                    a.add(parseIdentifier(boundVarIdent))
                    a
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
            SExpr.List.Empty -> throw ParseException("expected pair of ident, but found empty list!")
            SExpr.Nil -> throw ParseException("expected pair of ident, but found nil!")
        }
    }
}

fun ex2_29() {
    fun testSExpr(source: String) {
        val sExpr = SExpr.parse(source)
        println(sExpr)
        println(sExpr.toPrintableString())
        val lExpr = LExpr.parse(sExpr)
        println(lExpr)
        val sExprUnParsed = lExpr.unParse()
        println(sExprUnParsed)
        println(sExprUnParsed.toPrintableString())
        println()
    }

    val sources = arrayOf(
        "( lambda ( a b c ) ( add a b c ) )",
        "( ( lambda ( a ) ( a b ) ) c )",
        "( lambda ( x ) ( lambda ( y ) ( ( lambda ( x ) ( x y ) ) x ) ) )"
    )

    for (source in sources) {
        testSExpr(source)
    }
}
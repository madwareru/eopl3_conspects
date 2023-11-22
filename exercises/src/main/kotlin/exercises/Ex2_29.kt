package exercises

import java.util.Stack
sealed class SExpr {
    data class Identifier(val data: String) : SExpr()
    sealed class List : SExpr() {
        data object Empty: List()
        data class Pair(val head: SExpr, val tail: SExpr) : List() {
            fun <T> fold(init: T, step: (T, SExpr) -> T): T {
                val v = step(init, head)
                return if (tail !is Pair) { v } else { tail.fold(v, step) }
            }

            fun <T> toList(mapping: (SExpr) -> T): kotlin.collections.List<T> =
                fold(mutableListOf()) { a, n -> a.add(mapping(n)); a }
        }

        companion object {
            fun of(vararg entries: SExpr): List = when {
                entries.isEmpty() -> Empty
                else -> entries.indices.reversed().fold(Empty as List) { a, i -> Pair(entries[i], a) }
            }
        }
    }

    private sealed class Token {
        data object Whitespace: Token()
        data object LParen: Token()
        data object RParen: Token()
        data class Identifier(val data: String): Token()
    }
    class ScanException(errorMessage: String) : Exception(errorMessage)
    class ParseException(errorMessage: String) : Exception(errorMessage)

    fun toPrintableString(): String {
        return when (this) {
            is Identifier -> data
            List.Empty -> "( )"
            is List.Pair -> "( ${fold("") { a, n -> a + n.toPrintableString() + " " }})"
        }
    }

    companion object {
        private fun scan_error(errorMessage: String): Nothing = throw ScanException(errorMessage)

        private fun scan(source: String): kotlin.collections.List<Token> {
            fun CharIterator.tryNext(): Option<Char> = if (!hasNext()) {
                none()
            } else {
                some { nextChar() }
            }

            val charIterator = source.iterator()
            val stream = mutableListOf<Token>()

            var eatenChar: Option<Char> = none()

            while (true) {
                val next = when (eatenChar) {
                    is Option.None -> charIterator.tryNext()
                    is Option.Some -> { val c = eatenChar; eatenChar = none(); c }
                }

                when (next) {
                    is Option.None -> break
                    is Option.Some -> {
                        val nc = next.value
                        when {
                            nc == '(' -> stream.add(Token.LParen)
                            nc == ')' -> stream.add(Token.RParen)
                            nc.isWhitespace() -> {
                                while (true) {
                                    when(val c = charIterator.tryNext()) {
                                        is Option.None -> {
                                            stream.add(Token.Whitespace)
                                            break
                                        }
                                        is Option.Some -> {
                                            if (!c.value.isWhitespace()) {
                                                eatenChar = c
                                                stream.add(Token.Whitespace)
                                                break
                                            }
                                        }
                                    }
                                }
                            }
                            nc.isLetter() || nc == ':' || nc == '_' -> {
                                var tokenData = String(charArrayOf(nc))
                                while (true) {
                                    when (val c = charIterator.tryNext()) {
                                        is Option.None -> stream.add(Token.Identifier(tokenData))
                                        is Option.Some -> when {
                                            !(
                                                c.value.isLetter() ||
                                                c.value.isDigit() ||
                                                arrayOf('-', '_', '!', '?', ':').contains(c.value)
                                            ) -> {
                                                eatenChar = c
                                                stream.add(Token.Identifier(tokenData))
                                            }
                                            else -> {
                                                tokenData += c.value
                                                continue
                                            }
                                        }
                                    }
                                    break
                                }
                            }
                            else -> scan_error("found a bad character")
                        }
                    }
                }

            }

            return stream
        }

        private fun parse_error(errorMessage: String): Nothing = throw ParseException(errorMessage)

        fun parse(source: String): SExpr {
            val tokens = scan(source)
            var contents = mutableListOf<SExpr>()
            val groupStack: Stack<MutableList<SExpr>> = Stack()
            var justStarted = true

            var result: Option<SExpr> = none()

            for (token in tokens) {
                if (token == Token.Whitespace) { continue }

                if (!result.isNone) { parse_error("found tokens after a closing paren") }

                if (justStarted) {
                    when (token) {
                        is Token.Identifier -> result = some { Identifier(token.data) }
                        Token.LParen -> {}
                        else -> parse_error("s-expr is expected to be a single identifier or be opened by '('")
                    }
                    justStarted = false
                    continue
                }

                when (token) {
                    Token.LParen -> { groupStack.push(contents); contents = mutableListOf() }
                    Token.RParen -> {
                        val resultContent = when {
                            contents.isNotEmpty() -> contents
                                .indices
                                .reversed()
                                .fold(List.Empty as List) { a, i -> List.Pair(contents[i], a) }
                            else -> List.Empty
                        }

                        if (!groupStack.isEmpty()) {
                            contents = groupStack.pop()
                            contents.add(resultContent)
                        } else {
                            result = some { resultContent }
                        }
                    }
                    is Token.Identifier -> contents.add(Identifier(token.data))
                    else -> {}
                }
            }

            return result.unwrapOr { parse_error("s-expr is expected to be closed by ')'") }
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
            this.isNotEmpty() -> indices
                .reversed()
                .fold(SExpr.List.Empty as SExpr.List) { a, i -> SExpr.List.Pair(this[i].unParse(), a) }
            else -> SExpr.List.Empty
        }

        return when (this) {
            is Application -> SExpr.apply(rator.unParse(), rands.toSExpr())
            is Identifier -> SExpr.Identifier(data)
            is Lambda -> SExpr.lambda(boundVars.toSExpr(), body.unParse())
        }
    }

    companion object {
        private fun parse_error(errorMessage: String): Nothing = throw ParseException(errorMessage)

        private fun parseApplication(datum: SExpr.List.Pair): Application {
            val randsList = datum.tail.tryCast<SExpr.List.Pair>().unwrapOr { parse_error("rands should be pair!") }
            return Application(parse(datum.head), randsList.toList { parse(it) })
        }

        private fun parseLambda(datum: SExpr): Lambda {
            val (l, r) = datum.tryCast<SExpr.List.Pair>().unwrapOr { parse_error("expected pair, found $datum") }
            val bounds = l.tryCast<SExpr.List.Pair>().unwrapOr { parse_error("expected pair, found $l") }
            val bodyList = r.tryCast<SExpr.List.Pair>().unwrapOr { parse_error("expected pair, found $r") }
            if (bodyList.tail.tryCast<SExpr.List.Empty>().isNone ) { parse_error("found garbage in tail of lambda expr") }
            return Lambda(
                bounds.toList {
                    it.tryCast<SExpr.Identifier>()
                        .map(::parseIdent)
                        .unwrapOr { parse_error("expected identifier, found $it") }
                },
                parse(bodyList.head)
            )
        }

        private fun parseIdent(datum: SExpr.Identifier): Identifier {
            if (datum.data == "lambda") { parse_error("found \"lambda\" identifier in incorrect place!") }
            return Identifier(datum.data)
        }

        private fun parseApplicationOrLambda(datum: SExpr.List.Pair): LExpr = when {
            datum.head is SExpr.Identifier && datum.head.data == "lambda" -> parseLambda(datum.tail)
            else -> parseApplication(datum)
        }

        fun parse(datum: SExpr): LExpr = when(datum) {
            is SExpr.Identifier -> parseIdent(datum)
            is SExpr.List.Pair -> parseApplicationOrLambda(datum)
            SExpr.List.Empty -> parse_error("expected pair or ident, found empty list")
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
        "( lambda ( a-one b:two c? ) ( add! a-one b:two c? ) )",
        "( lambda ( a b c ) ( add a b c ) )",
        "( ( lambda ( a ) ( a b ) ) c )",
        "( lambda ( x ) ( lambda ( y ) ( ( lambda ( x ) ( x y ) ) x ) ) )"
    )

    for (source in sources) {
        testSExpr(source)
    }
}
package exercises

import com.github.h0tk3y.betterParse.combinators.*
import com.github.h0tk3y.betterParse.grammar.Grammar
import com.github.h0tk3y.betterParse.grammar.parseToEnd
import com.github.h0tk3y.betterParse.grammar.parser
import com.github.h0tk3y.betterParse.lexer.literalToken
import com.github.h0tk3y.betterParse.lexer.regexToken
import com.github.h0tk3y.betterParse.parser.Parser

private fun List<SExpr>.toSList() =
    indices
        .reversed()
        .fold(SExpr.List.Empty as SExpr.List) { acc, i -> acc.pushFront(this[i]) }


object SExprGrammar : Grammar<SExpr>() {
    // scanning
    @Suppress("unused")
    private val _ws by regexToken("\\s+", ignore = true)

    private val lParen by literalToken("(")
    private val rParen by literalToken(")")
    private val id by regexToken("([:_]|\\w)([:_\\-!?]|\\w)*")

    // parsing
    private val parseIdentifier = id
        .use { SExpr.Identifier(text) }

    private val parseList = (-lParen * zeroOrMore(parser { sExprValue }) * -rParen)
        .use { toSList() }

    private val sExprValue: Parser<SExpr> by parseIdentifier or parseList

    override val rootParser by sExprValue
}

fun ex2_29_v2() {
    fun testSExpr(source: String) {
        val sExpr = SExprGrammar.parseToEnd(source)
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

    for (source in sources) testSExpr(source)
}
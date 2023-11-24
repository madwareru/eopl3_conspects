package exercises

import com.github.h0tk3y.betterParse.combinators.*
import com.github.h0tk3y.betterParse.grammar.Grammar
import com.github.h0tk3y.betterParse.grammar.parseToEnd
import com.github.h0tk3y.betterParse.grammar.parser
import com.github.h0tk3y.betterParse.lexer.literalToken
import com.github.h0tk3y.betterParse.lexer.regexToken
import com.github.h0tk3y.betterParse.parser.Parser

object SExprGrammar : Grammar<SExpr>() {
    private val lParen by literalToken("(")
    private val rParen by literalToken(")")
    @Suppress("unused")
    private val _ws by regexToken("\\s+", ignore = true)
    private val id by regexToken("([:_]|\\w)([:_\\-!?]|\\w)*")
    private val term: Parser<SExpr> by
    (id use { SExpr.Identifier(text) }) or
            ((-lParen * oneOrMore(parser(this::term)) * -rParen)
                    map { it.indices.reversed().fold(SExpr.List.Empty as SExpr.List) { acc, i -> acc.pushFront(it[i]) } }
                    )
    override val rootParser by term
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

    for (source in sources) {
        testSExpr(source)
    }
}
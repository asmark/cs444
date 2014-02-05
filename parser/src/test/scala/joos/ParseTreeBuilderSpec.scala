package joos

import joos.exceptions.JoosParseException
import joos.tokens.TokenKind.TokenKindValue
import joos.tokens.{TokenKind, Token}
import org.scalatest.{Matchers, FlatSpec}

class ParseTreeBuilderSpec extends FlatSpec with Matchers {

  final val sampleFileName = "/sample.lr1"
  final val actionTable = LrOneReader(getClass.getResourceAsStream(sampleFileName)).actionTable

  def newToken(lexeme: String): Token = Token(new TokenKindValue(lexeme, () => null), lexeme)


  "A valid sequence of tokens" should "result in a left-deep parse tree" in {
    val parseTreeBuilder = ParseTreeBuilder(actionTable)

    val tree = parseTreeBuilder.build(
      Seq(
        newToken("LeftParen"), newToken("Id"), newToken("Minus"),
        newToken("LeftParen"), newToken("Id"), newToken("Minus"),
        newToken("Id"), newToken("RightParen"), newToken("RightParen")
      )
    ).levelOrder

    tree.length shouldEqual 8
    tree(0) shouldEqual Seq("expr")
    tree(1) shouldEqual Seq("term")
    tree(2) shouldEqual Seq("(", "expr", ")")
    tree(3) shouldEqual Seq("expr", "-", "term")
    tree(4) shouldEqual Seq("term", "(", "expr", ")")
    tree(5) shouldEqual Seq("Identifier", "expr", "-", "term")
    tree(6) shouldEqual Seq("term", "Identifier")
    tree(7) shouldEqual Seq("Identifier")
  }

  "An invalid sequence of tokens" should "result in an exception" in {
    val parseTreeBuilder = ParseTreeBuilder(actionTable)

    intercept[JoosParseException] {
      parseTreeBuilder.build(
        Seq(
          newToken("LeftParen"), newToken("Id"), newToken("Minus"),
          newToken("Id"), newToken("RightParen"), newToken("RightParen")
        )
      )
    }
  }
}

package joos

import java.io.FileInputStream
import joos.exceptions.JoosParseException
import joos.tokens.Token
import joos.tokens.TokenKind.TokenKindValue
import org.scalatest.{Matchers, FlatSpec}
import joos.tokens.{TokenKind, Token}

class ParseTreeBuilderSpec extends FlatSpec with Matchers {

  final val sampleFileName = "/sample.lr1"
  final val actionTable = LrOneReader(getClass.getResourceAsStream(sampleFileName)).actionTable

  def newToken(lexeme: String): Token = Token(new TokenKindValue(lexeme, () => null), lexeme)


  "A valid sequence of tokens" should "result in a left-deep parse tree" in {
    val parseTreeBuilder = ParseTreeBuilder(actionTable)

    val tree = parseTreeBuilder.build(
      Seq(
        newToken("("), newToken("id"), newToken("-"),
        newToken("("), newToken("id"), newToken("-"),
        newToken("id"), newToken(")"), newToken(")")
      )
    ).levelOrder

    tree.length shouldEqual 8
    tree(0) shouldEqual Seq("expr")
    tree(1) shouldEqual Seq("term")
    tree(2) shouldEqual Seq("(", "expr", ")")
    tree(3) shouldEqual Seq("expr", "-", "term")
    tree(4) shouldEqual Seq("term", "(", "expr", ")")
    tree(5) shouldEqual Seq("id", "expr", "-", "term")
    tree(6) shouldEqual Seq("term", "id")
    tree(7) shouldEqual Seq("id")
  }

  "An invalid sequence of tokens" should "result in an exception" in {
    val parseTreeBuilder = ParseTreeBuilder(actionTable)

    intercept[JoosParseException] {
      parseTreeBuilder.build(
        Seq(
          newToken("("), newToken("id"), newToken("-"),
          newToken("id"), newToken(")"), newToken(")")
        )
      )
    }
  }

  final val joosFile = "/grammar.lr1"
  final val grammar = LrOneReader(getClass.getResourceAsStream(joosFile))
  final val aTable = grammar.actionTable

  "Build parse tree" should "B p t" in {
    val parseTreeBuilder = ParseTreeBuilder(aTable)

    parseTreeBuilder.build(Seq(
      Token(TokenKind.Package, "package"), Token(TokenKind.Id, "test.pkg"), Token(TokenKind.SemiColon, ";"),
      Token(TokenKind.Public, "public"), Token(TokenKind.Class, "class"), Token(TokenKind.Id, "main"), Token(TokenKind.LeftBrace, "{"), Token(TokenKind.RightBrace, "}")))

    println(aTable.startSymbol)
  }


}

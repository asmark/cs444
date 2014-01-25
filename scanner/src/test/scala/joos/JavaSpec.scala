package joos

import joos.automata.DfaNode
import joos.regexp.Atom
import joos.scanner.Scanner
import joos.tokens.TokenKind
import joos.tokens.TokenKind._
import org.scalatest.{FlatSpec, Matchers}
import scala.collection.Seq
import scala.language.postfixOps

class JavaSpec extends FlatSpec with Matchers {

  // We need to add a whitespace multi state at the start. This is ugly, where do we want to do this?
  val javaRegexp = (Atom(' ') *) + TokenKind.values.map(_.asInstanceOf[TokenKindValue].getRegexp()).reduceRight((a, b) => a | b)
  val javaDfa = DfaNode(javaRegexp)

  behavior of "Parsing java programs"

  // Later this will just take java files from a directory parse them, compare with expected output
  it should "tokenize class declaration correctly" in {
    val scanner = Scanner(javaDfa)

    "public class Main { public static void main(String[] args) { System.out.println(\"\"); }}".foreach(scanner.parse(_))

    scanner.getTokens().map(_.kind) should contain
    theSameElementsInOrderAs(Seq(Public, Class, Id, LeftBrace, Public, Static, Void, Id, LeftParen, Id, LeftBracket, RightBracket, Id, RightParen, LeftBrace, Id, Dot, Id, Dot, Id, LeftParen, String, RightParen, SemiColon, RightBrace, RightBrace))

  }

}

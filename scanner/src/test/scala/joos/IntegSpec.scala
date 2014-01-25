package joos

import joos.automata.DfaNode
import joos.scanner.Scanner
import joos.tokens.TokenKind
import joos.tokens.TokenKind.TokenKindValue
import org.scalatest.{FlatSpec, Matchers}
import scala.io.Source
import scala.language.postfixOps

class IntegSpec extends FlatSpec with Matchers {
  final val casesDirectory = "/cases"
  final val expectDirectory = "/expect"

  def getSource(dir: String, file: String): Source = Source.fromURL(getClass.getResource(dir + "/" + file))

  val JavaRegexp = TokenKind.values.map(_.asInstanceOf[TokenKindValue].getRegexp()).reduceRight((a, b) => a | b)
  val JavaDfa = DfaNode(JavaRegexp)

  behavior of "Parsing java programs"
  Source.fromURL(getClass.getResource(casesDirectory)).getLines().foreach {
    file =>
      it should s"tokenize ${file}" in {
        val scanner = Scanner(JavaDfa)
        val tokens = scanner.tokenize(getSource(casesDirectory, file))

        val expect = getSource(expectDirectory, file + ".expect")

        // Filter out whitespace tokens
        tokens.withFilter(_.kind != TokenKind.Whitespace).map(_.kind.asInstanceOf[TokenKindValue].getName()) should contain theSameElementsInOrderAs
          expect.getLines().toSeq
      }
  }


}

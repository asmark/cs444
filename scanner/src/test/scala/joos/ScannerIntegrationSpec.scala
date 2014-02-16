package joos

import joos.automata.Dfa
import joos.exceptions.ScanningException
import joos.scanner.Scanner
import joos.tokens.TokenKind
import joos.tokens.TokenKind.TokenKindValue
import org.scalatest.{FlatSpec, Matchers}
import scala.io.Source
import scala.language.postfixOps
import java.io.FileInputStream

class ScannerIntegrationSpec extends FlatSpec with Matchers {
  final val casesDirectory = "/cases"
  final val expectDirectory = "/expect"

  def getSource(dir: String) = Source.fromURL(getClass.getResource(dir))

  def getSource(dir: String, file: String) = Source.fromURL(getClass.getResource(dir + "/" + file))

  lazy val JavaDfa = Dfa.deserialize(new FileInputStream(resources.lexerDfa))

  behavior of "Scanning java programs (checked)"
  getSource(casesDirectory).getLines().foreach {
    file =>
      it should s"tokenize ${file} and check" in {
        val scanner = Scanner(JavaDfa)
        val tokens = scanner.tokenize(getSource(casesDirectory, file))

        val expect = getSource(expectDirectory, file + ".expect")

        // Filter out whitespace tokens
        tokens.withFilter(_.kind != TokenKind.Whitespace).map(t => t.kind + " " + t.lexeme) should contain theSameElementsInOrderAs
          expect.getLines().toList
      }
  }

}

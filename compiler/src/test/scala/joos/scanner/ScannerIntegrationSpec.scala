package joos.scanner

import java.io.FileInputStream
import joos.automata.Dfa
import joos.resources
import joos.test.tags.IntegrationTest
import joos.tokens.TokenKind
import org.scalatest.{FlatSpec, Matchers}
import scala.io.Source
import scala.language.postfixOps

class ScannerIntegrationSpec extends FlatSpec with Matchers {
  final val casesDirectory = "/scanner/cases"
  final val expectDirectory = "/scanner/expect"

  def getSource(dir: String) = Source.fromURL(getClass.getResource(dir))

  def getSource(dir: String, file: String) = Source.fromURL(getClass.getResource(dir + "/" + file))

  lazy val JavaDfa = Dfa.deserialize(new FileInputStream(resources.lexerDfa))

  behavior of "Scanning java programs (checked)"
  getSource(casesDirectory).getLines().foreach {
    file =>
      it should s"tokenize ${file} and check" taggedAs IntegrationTest in {
        val scanner = Scanner(JavaDfa)
        val tokens = scanner.tokenize(getSource(casesDirectory, file))

        val expect = getSource(expectDirectory, file + ".expect")

        // Filter out whitespace tokens
        tokens.withFilter(_.kind != TokenKind.Whitespace).map(t => t.kind + " " + t.lexeme) should contain theSameElementsInOrderAs
            expect.getLines().toList
      }
  }

}

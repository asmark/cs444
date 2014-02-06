package joos.a1

import java.io.FileInputStream
import joos.automata.Dfa
import joos.exceptions.JoosParseException
import joos.parsetree.ParseTree
import joos.resources
import joos.scanner.Scanner
import joos.tokens.Token
import joos.{ParseTreeBuilder, LrOneReader}
import scala.io.Source

object ScanParseWeed {

  private def tokenize(path: String): Seq[Token] = {
    val joosDfa = Dfa.deserialize(new FileInputStream(resources.lexerDfa))
    val scanner = Scanner(joosDfa)

    val source = Source.fromFile(path)
    val tokens = scanner.tokenize(source)
    source.close()
    tokens
  }

  private def parse(tokens: Seq[Token]): ParseTree = {
    val actionTable = LrOneReader(
      new FileInputStream(resources.lalr1Table),
      new FileInputStream(resources.serializedGrammar)
    )
    ParseTreeBuilder(actionTable).build(tokens)
  }

  def apply(path: String): Int = {
    try {
      val tokens = tokenize(path)
      parse(tokens)
    } catch {
      case jpe: JoosParseException => {
        jpe.printStackTrace(); return 42
      }
    }
    return 0
  }

}

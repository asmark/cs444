package joos.a1

import joos.automata.Dfa
import joos.exceptions.JoosParseException
import joos.parsetree.ParseTree
import joos.scanner.Scanner
import joos.tokens.Token
import joos.{ParseTreeBuilder, LrOneReader}
import scala.io.Source

object ScanParseWeed {

  // TODO: Get this from properties
  final val DfaFile = "/joos-1w-dfa.dfa"
  final val LrOneFile = "/joos-1w-grammar.lr1"

  private def tokenize(path: String): Seq[Token] = {
    val joosDfa = Dfa.deserialize(getClass.getResourceAsStream(DfaFile))
    val scanner = Scanner(joosDfa)

    val source = Source.fromFile(path)
    val tokens = scanner.tokenize(source)
    source.close()
    tokens
  }

  private def parse(tokens: Seq[Token]): ParseTree = {
    val actionTable = LrOneReader(getClass.getResourceAsStream(LrOneFile)).actionTable
    ParseTreeBuilder(actionTable).build(tokens)
  }

  def apply(path: String): Int = {
    try {
      val tokens = tokenize(path)
      parse(tokens)
    } catch {
      case jpe: JoosParseException => return 42
    }
    return 0
  }

}

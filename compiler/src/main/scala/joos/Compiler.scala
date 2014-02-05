package joos

import joos.automata.Dfa
import joos.exceptions.JoosParseException
import joos.parsetree.ParseTree
import joos.scanner.Scanner
import joos.tokens.Token
import scala.io.Source

object Compiler {

  // TODO: Get this from properties
  final val DfaFile = "/joos-1w-dfa.dfa"
  final val LrOneFile = "/joos-1w-grammar.lr1"

  def main(args: Array[String]) {
    try {
      val tokens = tokenize(args(0))
      val parseTree = parse(tokens)
      println(parseTree.levelOrder)
    } catch {
      case jpe: JoosParseException => 42
    }
    0
  }

  def tokenize(path: String): Seq[Token] = {
    val joosDfa = Dfa.deserialize(getClass.getResourceAsStream(DfaFile))
    val scanner = Scanner(joosDfa)

    val source = Source.fromFile(path)
    val tokens = scanner.tokenize(source)
    source.close()
    tokens
  }

  def parse(tokens: Seq[Token]): ParseTree = {
    val actionTable = LrOneReader(getClass.getResourceAsStream(LrOneFile)).actionTable
    ParseTreeBuilder(actionTable).build(tokens)
  }
}

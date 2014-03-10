package joos.syntax

import java.io.{File, FileInputStream}
import joos.ast.AbstractSyntaxTree
import joos.resources
import joos.syntax.automata.Dfa
import joos.syntax.parser.{ParseMetaData, ParseTreeBuilder, LrOneReader}
import joos.syntax.parsetree.ParseTree
import joos.syntax.scanner.Scanner
import joos.syntax.tokens.TerminalToken
import joos.syntax.weeder.Weeder
import scala.io.Source

object SyntaxCheck {

  lazy val joosDfa = Dfa.deserialize(new FileInputStream(resources.lexerDfa))
  lazy val actionTable = LrOneReader(new FileInputStream(resources.lalr1Table)).actionTable

  private[this] def tokenize(path: String): Seq[TerminalToken] = {
    val scanner = Scanner(joosDfa)

    val source = Source.fromFile(path)
    val tokens = scanner.tokenize(source)
    source.close()
    tokens
  }

  private def parse(tokens: Seq[TerminalToken]): ParseTree = {
    ParseTreeBuilder(actionTable).build(tokens)
  }

  private def weed(parseTree: ParseTree, metaData: ParseMetaData) {
    Weeder.weed(parseTree, metaData)
  }

  def apply(path: String): AbstractSyntaxTree = {
    val metaData = ParseMetaData(path)
    val tokens = tokenize(path)
    val parseTree = parse(tokens)
    weed(parseTree, metaData)
    AbstractSyntaxTree(parseTree)
  }

  def apply(file: File): AbstractSyntaxTree = {
    apply(file.getAbsolutePath)
  }
}

package joos.syntax

import java.io.{PrintWriter, StringWriter, FileInputStream}
import joos.ast.{AstConstructionException, AbstractSyntaxTree}
import joos.core.Logger
import joos.resources
import joos.syntax.automata.Dfa
import joos.syntax.parser.{JoosParseException, ParseMetaData, ParseTreeBuilder, LrOneReader}
import joos.syntax.parsetree.ParseTree
import joos.syntax.tokens.TerminalToken
import joos.syntax.weeder.{WeederException, Weeder}
import scala.io.Source
import joos.syntax.scanner.{ScanningException, Scanner}

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

  def apply(path: String): Option[AbstractSyntaxTree] = {
    try {
      val metaData = ParseMetaData(path)
      val tokens = tokenize(path)
      val parseTree = parse(tokens)
      weed(parseTree, metaData)
      Some(AbstractSyntaxTree(parseTree))
    } catch {
      case e @ (_:ScanningException | _:WeederException | _:JoosParseException | _:AstConstructionException) => {
        val errors = new StringWriter()
        e.printStackTrace(new PrintWriter(errors))
        Logger.logError(errors.toString)
        return None
      }
    }
  }
}

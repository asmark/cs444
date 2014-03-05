package joos.a1

import java.io.{PrintWriter, StringWriter, FileInputStream}
import joos.automata.Dfa
import joos.parser.{JoosParseException, ParseMetaData, ParseTreeBuilder, LrOneReader}
import joos.parsetree.ParseTree
import joos.resources
import joos.scanner.{ScanningException, Scanner}
import joos.tokens.TerminalToken
import joos.weeder.{WeederException, Weeder}
import scala.io.Source
import joos.ast.{AstConstructionException, AbstractSyntaxTree}
import joos.core.Logger

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

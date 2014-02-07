package joos.a1

import java.io.FileInputStream
import joos.automata.Dfa
import joos.exceptions.ScanningException
import joos.parser.exceptions.JoosParseException
import joos.parser.{ParseTreeBuilder, LrOneReader}
import joos.parsetree.ParseTree
import joos.resources
import joos.scanner.Scanner
import joos.tokens.TerminalToken
import joos.weeder.Weeder
import joos.weeder.exceptions.WeederException
import scala.io.Source

object ScanParseWeed {

  private[this] def tokenize(path: String): Seq[TerminalToken] = {
    val joosDfa = Dfa.deserialize(new FileInputStream(resources.lexerDfa))
    val scanner = Scanner(joosDfa)

    val source = Source.fromFile(path)
    val tokens = scanner.tokenize(source)
    source.close()
    tokens
  }

  private def parse(tokens: Seq[TerminalToken]): ParseTree = {
    val actionTable = LrOneReader(new FileInputStream(resources.lalr1Table)).actionTable
    ParseTreeBuilder(actionTable).build(tokens)
  }

  private def weed(parseTree: ParseTree) {
    Weeder(parseTree)
  }

  def apply(path: String): Int = {
    try {
      val tokens = tokenize(path)
      val parseTree = parse(tokens)
      weed(parseTree)
    } catch {
      case jpe: JoosParseException => {
        jpe.printStackTrace()
        return 42
      }
      case wde: WeederException => {
        wde.printStackTrace()
        return 42
      }
      case sce: ScanningException => {
        sce.printStackTrace()
        return 42
      }
    }
    return 0
  }
}

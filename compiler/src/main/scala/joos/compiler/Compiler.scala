package joos.compiler

import joos.core.Logger
import joos.semantic.{NameResolution, SemanticException}
import joos.syntax.{JoosSyntaxException, SyntaxCheck}

object Compiler {
  def main(args: Array[String]) {

    try {
      val asts = args map SyntaxCheck.apply
      NameResolution(asts)
    } catch {
      case e: JoosSyntaxException => {
        Logger.logWarning(s"${args} failed syntax check")
        Logger.logWarning(e.getMessage)
        sys.exit(42)
      }
      case e: SemanticException => {
        Logger.logWarning(s"${args} failed semantic check")
        Logger.logWarning(e.getMessage)
        sys.exit(42)
      }
    }
    sys.exit(0)
  }
}

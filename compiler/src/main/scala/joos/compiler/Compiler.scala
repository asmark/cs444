package joos.compiler

import joos.core.Logger
import joos.semantic.{NameResolution, SemanticException}
import joos.syntax.{JoosSyntaxException, SyntaxCheck}

object Compiler {
  def main(args: Array[String]) {
    sys.exit(compile(args))
  }

  def compile(files: Array[String]) = {
    try {
      val asts = files map SyntaxCheck.apply
      NameResolution(asts)
      0
    } catch {
      case e: JoosSyntaxException => {
        Logger.logWarning(s"${files} failed syntax check")
        Logger.logWarning(e.getMessage)
        42
      }
      case e: SemanticException => {
        Logger.logWarning(s"${files} failed semantic check")
        Logger.logWarning(e.getMessage)
        42
      }
    }
  }
}

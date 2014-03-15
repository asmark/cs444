package joos.compiler

import joos.core.Logger
import joos.semantic.{TypeChecking, NameResolution, SemanticException}
import joos.syntax.{JoosSyntaxException, SyntaxCheck}

object Compiler {
  def main(args: Array[String]) {
    sys.exit(compile(args))
  }

  def compile(files: Array[String]) = {
    try {
      val asts = files map SyntaxCheck.apply
      NameResolution(asts)
      TypeChecking(asts)
      0
    } catch {
      case e: CompilationException => {
        Logger.logWarning(e.getMessage)
        42
      }
    }
  }
}

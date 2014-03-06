package joos.compiler

import joos.semantic.{NameResolution, SemanticException}
import joos.syntax.SyntaxCheck

object Compiler {
  def main(args: Array[String]) {

    try {
      val asts = args map SyntaxCheck.apply collect {
        case None => sys.exit(42)
        case Some(ast) => ast
      }

      try {
        NameResolution(asts)
      } catch {
        case e: SemanticException => sys.exit(42)
      }
    }
    sys.exit(0)
  }
}

package joos

import joos.a1.SyntaxCheck
import joos.a2.SemanticCheck
import joos.semantic.SemanticException

object Compiler {
  def main(args: Array[String]) {

    try {
      val asts = args map SyntaxCheck.apply collect {
        case None => sys.exit(42)
        case Some(ast) => ast
      }

      try {
        SemanticCheck(asts)
      } catch {
        case e: SemanticException => sys.exit(42)
      }
    }
  }
}

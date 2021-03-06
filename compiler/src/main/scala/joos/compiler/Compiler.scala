package joos.compiler

import joos.core.Logger
import joos.semantic.{StaticAnalysis, TypeChecking, NameResolution}
import joos.syntax.SyntaxCheck
import joos.codegen.CodeGeneration

object Compiler {
  def main(args: Array[String]) {
    sys.exit(compile(args))
  }

  def compile(files: Array[String]) = {
    try {
      val asts = files map SyntaxCheck.apply
      NameResolution(asts)
      TypeChecking(asts)
      StaticAnalysis(asts)
      CodeGeneration(asts)
      0
    } catch {
      case e: CompilationException => {
        Logger.logError(e.getMessage)
        42
      }
    }
  }
}

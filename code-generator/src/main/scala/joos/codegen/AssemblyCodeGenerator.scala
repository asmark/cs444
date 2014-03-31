package joos.codegen

import joos.ast.AstNode
import scala.language.implicitConversions

trait AssemblyCodeGenerator {
  def generate()
}

object AssemblyCodeGenerator {
  implicit def toAssemblyCodeGenerator(element: AstNode)
      (implicit environment: AssemblyCodeGeneratorEnvironment): AssemblyCodeGenerator = {
    // TODO: implement
    null
  }
}

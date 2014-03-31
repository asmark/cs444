package joos.codegen.generators

import joos.assemgen.AssemblyLine
import joos.ast.AstNode
import joos.codegen.AssemblyCodeGeneratorEnvironment
import scala.language.implicitConversions

trait AssemblyCodeGenerator {
  def generate()

  def environment: AssemblyCodeGeneratorEnvironment

  def appendText(lines: AssemblyLine*) {
    environment.assemblyManager.appendData(lines: _*)
  }

  def appendGlobal(lines: AssemblyLine*) {
    environment.assemblyManager.appendGlobal(lines: _*)
  }
}

object AssemblyCodeGenerator {
  implicit def toAssemblyCodeGenerator(element: AstNode)
      (implicit environment: AssemblyCodeGeneratorEnvironment): AssemblyCodeGenerator = {
    // TODO: implement
    null
  }
}

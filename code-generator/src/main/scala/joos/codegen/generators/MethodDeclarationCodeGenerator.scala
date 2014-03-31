package joos.codegen.generators

import joos.ast.declarations.MethodDeclaration
import AssemblyCodeGenerator._
import joos.codegen.AssemblyCodeGeneratorEnvironment

class MethodDeclarationCodeGenerator(method: MethodDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    // TODO: Generate prologue
    method.body.foreach(_.generate())
    // TODO: Generate epilogue
  }
}

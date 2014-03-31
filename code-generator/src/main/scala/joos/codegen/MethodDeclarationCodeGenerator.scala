package joos.codegen

import joos.ast.declarations.MethodDeclaration
import joos.codegen.AssemblyCodeGenerator._

class MethodDeclarationCodeGenerator(method: MethodDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    // TODO: Generate prologue
    method.body.foreach(_.generate())
    // TODO: Generate epilogue
  }
}

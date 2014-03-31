package joos.codegen.generators

import joos.ast.declarations.MethodDeclaration
import joos.codegen.AssemblyCodeGeneratorEnvironment

class MethodDeclarationCodeGenerator(method: MethodDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  // Convention: the pointer to the current object is stored in ECX
  override def generate() {
    // TODO: Generate prologue
    method.body.foreach(_.generate())
    // TODO: Generate epilogue
  }
}

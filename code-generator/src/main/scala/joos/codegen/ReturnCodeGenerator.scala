package joos.codegen

import joos.ast.statements.ReturnStatement

class ReturnCodeGenerator(statement: ReturnStatement)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {
  
  override def generate() {

  }
}

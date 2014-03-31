package joos.codegen.generators

import joos.assemgen._
import joos.ast.statements.ReturnStatement
import joos.codegen.{AssemblyCodeGenerator, AssemblyCodeGeneratorEnvironment}

class ReturnCodeGenerator(statement: ReturnStatement)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {
  
  override def generate() {
    appendText(
      leave,
      ret
    )
  }
}

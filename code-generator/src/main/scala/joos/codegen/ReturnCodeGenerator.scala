package joos.codegen

import joos.ast.statements.ReturnStatement
import joos.assemgen._

class ReturnCodeGenerator(statement: ReturnStatement)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {
  
  override def generate() {
    appendText(
      leave,
      ret
    )
  }
}

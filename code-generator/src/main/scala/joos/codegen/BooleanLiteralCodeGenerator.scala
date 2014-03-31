package joos.codegen

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.BooleanLiteral

class BooleanLiteralCodeGenerator(literal: BooleanLiteral)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    appendText(
      mov(Eax, literal.value, s"Assemble Boolean literal: ${literal}")
    )
  }
}

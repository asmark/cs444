package joos.codegen

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.CharacterLiteral

class CharacterLiteralCodeGenerator(literal: CharacterLiteral)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    appendText(
      mov(Eax, literal.value, s"Assemble character literal: ${literal}")
    )
  }
}

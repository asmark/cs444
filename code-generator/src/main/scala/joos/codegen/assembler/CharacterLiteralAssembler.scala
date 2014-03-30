package joos.codegen.assembler

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.CharacterLiteral

class CharacterLiteralAssembler(literal: CharacterLiteral) extends Assembler {

  override def generateAssembly() {
    Seq(
      mov(Eax, literal.value)
    )
  }

}

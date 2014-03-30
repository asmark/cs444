package joos.codegen.assembler

import joos.ast.expressions.IntegerLiteral
import joos.assemgen._
import joos.assemgen.Register._

class IntegerLiteralAssembler(literal: IntegerLiteral) extends Assembler {

  override def generateAssembly() {
    Seq(
      mov(Eax, literal.value)
    )
  }

}

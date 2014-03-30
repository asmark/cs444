package joos.codegen.assembler

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.{BooleanLiteral, IntegerLiteral}

class BooleanLiteralAssembler(literal: BooleanLiteral) extends Assembler {

  override def generateAssembly() {
    Seq(
      mov(Eax, literal.value)
    )
  }

}

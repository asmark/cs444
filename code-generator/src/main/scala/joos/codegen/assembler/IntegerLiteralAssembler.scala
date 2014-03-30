package joos.codegen.assembler

import joos.ast.expressions.IntegerLiteral
import joos.assemgen._
import joos.assemgen.Register._
import joos.codegen.AssemblyFileManager

class IntegerLiteralAssembler(literal: IntegerLiteral)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {

  override def generateAssembly() {
    Seq(
      mov(Eax, literal.value)
    )
  }

}

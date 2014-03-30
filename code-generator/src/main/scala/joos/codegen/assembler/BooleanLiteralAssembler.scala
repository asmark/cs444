package joos.codegen.assembler

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.BooleanLiteral
import joos.codegen.AssemblyFileManager

class BooleanLiteralAssembler(literal: BooleanLiteral)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {

  override def generateAssembly() {
    Seq(
      mov(Eax, literal.value)
    )
  }

}

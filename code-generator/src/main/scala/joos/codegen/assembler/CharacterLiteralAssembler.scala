package joos.codegen.assembler

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.CharacterLiteral
import joos.codegen.AssemblyFileManager

class CharacterLiteralAssembler(literal: CharacterLiteral)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {

  override def generateAssembly() {
    Seq(
      mov(Eax, literal.value)
    )
  }

}

package joos.codegen.assembler

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.CharacterLiteral
import joos.codegen.AssemblyFileManager

class CharacterLiteralAssembler(literal: CharacterLiteral)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {

  override def generateAssembly() {
    assemblyManager.text = assemblyManager.text :+ mov(Eax, literal.value, s"Assemble character literal: ${literal}")
  }

}

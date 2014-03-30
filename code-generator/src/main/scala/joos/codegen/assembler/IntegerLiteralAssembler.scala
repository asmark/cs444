package joos.codegen.assembler

import joos.ast.expressions.IntegerLiteral
import joos.assemgen._
import joos.assemgen.Register._
import joos.codegen.AssemblyFileManager

class IntegerLiteralAssembler(literal: IntegerLiteral)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {

  override def generateAssembly() {
    assemblyManager.text = assemblyManager.text :+ mov(Eax, literal.value, s"Assemble integer literal: ${literal}")
  }

}

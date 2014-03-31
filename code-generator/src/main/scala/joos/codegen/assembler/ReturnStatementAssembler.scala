package joos.codegen.assembler

import joos.ast.statements.ReturnStatement
import joos.assemgen._
import joos.codegen.AssemblyFileManager

class ReturnStatementAssembler(statement: ReturnStatement)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly() {
    assemblyManager.appendText(
      leave,
      ret
    )
  }
}

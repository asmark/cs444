package joos.codegen.assembler

import joos.ast.statements.WhileStatement
import joos.codegen.AssemblyFileManager

class WhileStatementAssembler(statement: WhileStatement)(implicit val assemblyManager: AssemblyFileManager) extends Assembler{
  override def generateAssembly(): Unit = {}
}

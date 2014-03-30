package joos.codegen.assembler

import joos.ast.statements.IfStatement
import joos.codegen.AssemblyFileManager

class IfStatementAssembler(statement: IfStatement)(implicit val assemblyManager: AssemblyFileManager) extends Assembler{
  override def generateAssembly(): Unit = {}
}

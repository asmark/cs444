package joos.codegen.assembler

import joos.ast.statements.ForStatement
import joos.codegen.AssemblyFileManager

class ForStatementAssembler(statement: ForStatement)(implicit val assemblyManager: AssemblyFileManager) extends Assembler{
  override def generateAssembly(): Unit = {}
}

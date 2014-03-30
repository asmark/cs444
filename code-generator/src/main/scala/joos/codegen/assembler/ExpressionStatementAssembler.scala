package joos.codegen.assembler

import joos.ast.statements.ExpressionStatement
import joos.codegen.AssemblyFileManager

class ExpressionStatementAssembler(statement: ExpressionStatement)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

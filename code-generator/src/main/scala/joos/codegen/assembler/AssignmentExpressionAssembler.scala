package joos.codegen.assembler

import joos.ast.expressions.AssignmentExpression
import joos.codegen.AssemblyFileManager

class AssignmentExpressionAssembler(expression: AssignmentExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

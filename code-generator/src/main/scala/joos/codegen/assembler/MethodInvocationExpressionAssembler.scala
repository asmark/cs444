package joos.codegen.assembler

import joos.ast.expressions.MethodInvocationExpression
import joos.codegen.AssemblyFileManager

class MethodInvocationExpressionAssembler(expression: MethodInvocationExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

package joos.codegen.assembler

import joos.ast.expressions.InfixExpression
import joos.codegen.AssemblyFileManager

class InfixExpressionAssembler(expression: InfixExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

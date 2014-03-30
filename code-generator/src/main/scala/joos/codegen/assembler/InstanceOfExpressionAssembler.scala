package joos.codegen.assembler

import joos.ast.expressions.InstanceOfExpression
import joos.codegen.AssemblyFileManager

class InstanceOfExpressionAssembler(expression: InstanceOfExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

package joos.codegen.assembler

import joos.ast.expressions.ThisExpression
import joos.codegen.AssemblyFileManager

class ThisExpressionAssembler(expression: ThisExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

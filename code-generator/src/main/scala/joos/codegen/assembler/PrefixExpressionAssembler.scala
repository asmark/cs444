package joos.codegen.assembler

import joos.ast.expressions.PrefixExpression
import joos.codegen.AssemblyFileManager

class PrefixExpressionAssembler(expression: PrefixExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

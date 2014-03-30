package joos.codegen.assembler

import joos.ast.expressions.CastExpression
import joos.codegen.AssemblyFileManager

class CastExpressionAssembler(expression: CastExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

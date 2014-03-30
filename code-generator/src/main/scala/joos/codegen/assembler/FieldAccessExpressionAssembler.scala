package joos.codegen.assembler

import joos.ast.expressions.FieldAccessExpression
import joos.codegen.AssemblyFileManager

class FieldAccessExpressionAssembler(expression: FieldAccessExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

package joos.codegen.assembler

import joos.ast.expressions.ArrayCreationExpression
import joos.codegen.AssemblyFileManager

class ArrayCreationExpressionAssembler(expression: ArrayCreationExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler{
  override def generateAssembly(): Unit = {}
}

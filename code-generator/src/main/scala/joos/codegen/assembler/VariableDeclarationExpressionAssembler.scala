package joos.codegen.assembler

import joos.ast.expressions.VariableDeclarationExpression
import joos.codegen.AssemblyFileManager

class VariableDeclarationExpressionAssembler(expression: VariableDeclarationExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

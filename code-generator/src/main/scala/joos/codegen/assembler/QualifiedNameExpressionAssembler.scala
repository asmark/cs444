package joos.codegen.assembler

import joos.ast.expressions.QualifiedNameExpression
import joos.codegen.AssemblyFileManager

class QualifiedNameExpressionAssembler(expression: QualifiedNameExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler{
  override def generateAssembly(): Unit = {}
}

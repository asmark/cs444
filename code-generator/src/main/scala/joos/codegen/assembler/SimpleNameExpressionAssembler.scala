package joos.codegen.assembler

import joos.ast.expressions.SimpleNameExpression
import joos.codegen.AssemblyFileManager

class SimpleNameExpressionAssembler(expression: SimpleNameExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

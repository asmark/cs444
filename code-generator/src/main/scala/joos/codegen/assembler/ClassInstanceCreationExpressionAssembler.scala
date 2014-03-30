package joos.codegen.assembler

import joos.ast.expressions.ClassInstanceCreationExpression
import joos.codegen.AssemblyFileManager

class ClassInstanceCreationExpressionAssembler(expression: ClassInstanceCreationExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {
  }
}

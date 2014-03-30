package joos.codegen.assembler

import joos.ast.expressions.ClassInstanceCreationExpression
import joos.codegen.AssemblyFileManager
import joos.assemgen._

class ClassInstanceCreationExpressionAssembler(expression: ClassInstanceCreationExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {
    val typeDeclaration = expression.expressionType.declaration
    assemblyManager.globals.add(global(LabelReference(typeDeclaration.uniqueName)))
    
    typeDeclaration.containedMethods

  }
}

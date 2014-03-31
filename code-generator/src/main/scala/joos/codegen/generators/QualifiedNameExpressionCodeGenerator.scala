package joos.codegen.generators

import joos.ast.expressions.QualifiedNameExpression
import joos.codegen.AssemblyFileManager
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.{AssemblyCodeGenerator, AssemblyCodeGeneratorEnvironment}

class QualifiedNameExpressionCodeGenerator(expression: QualifiedNameExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler{
  override def generateAssembly(): Unit = {}
}

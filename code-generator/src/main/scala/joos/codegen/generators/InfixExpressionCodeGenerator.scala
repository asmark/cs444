package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.{AssemblyCodeGenerator, AssemblyCodeGeneratorEnvironment}
import joos.ast.expressions.InfixExpression
import joos.codegen.AssemblyFileManager

class InfixExpressionCodeGenerator(expression: InfixExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

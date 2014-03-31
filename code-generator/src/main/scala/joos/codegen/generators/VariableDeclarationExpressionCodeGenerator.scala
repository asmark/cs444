package joos.codegen.generators

import joos.ast.expressions.VariableDeclarationExpression
import joos.codegen.AssemblyFileManager
import joos.codegen.assembler.Assembler
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.{AssemblyCodeGenerator, AssemblyCodeGeneratorEnvironment}

class VariableDeclarationExpressionCodeGenerator(expression: VariableDeclarationExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

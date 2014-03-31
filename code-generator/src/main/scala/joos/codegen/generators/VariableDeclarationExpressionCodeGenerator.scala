package joos.codegen.generators

import joos.ast.expressions.VariableDeclarationExpression
import joos.codegen.AssemblyFileManager
import joos.codegen.assembler.Assembler
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment

class VariableDeclarationExpressionCodeGenerator(expression: VariableDeclarationExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {}

}

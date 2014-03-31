package joos.codegen.generators

import joos.ast.expressions.ThisExpression
import joos.codegen.AssemblyFileManager
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment

class ThisExpressionCodeGenerator(expression: ThisExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {}

}

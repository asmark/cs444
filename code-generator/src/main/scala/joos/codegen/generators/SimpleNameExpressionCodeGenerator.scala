package joos.codegen.generators

import joos.ast.expressions.SimpleNameExpression
import joos.codegen.AssemblyFileManager
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment

class SimpleNameExpressionCodeGenerator(expression: SimpleNameExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {}

}

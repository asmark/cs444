package joos.codegen.generators

import joos.ast.expressions.{NullLiteral, InstanceOfExpression}
import joos.codegen.AssemblyCodeGeneratorEnvironment

class NullLiteralCodeGenerator(expression: NullLiteral)
     (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {}

}

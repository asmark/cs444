package joos.codegen.generators

import joos.ast.expressions.ArrayAccessExpression
import joos.codegen.{AssemblyCodeGenerator, AssemblyCodeGeneratorEnvironment}

class ArrayAccessExpressionCodeGenerator(expression: ArrayAccessExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
  }
}

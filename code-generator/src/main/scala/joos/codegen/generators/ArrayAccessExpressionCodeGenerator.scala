package joos.codegen.generators

import joos.ast.expressions.ArrayAccessExpression
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.BooleanLiteral

class ArrayAccessExpressionCodeGenerator(expression: ArrayAccessExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    expression.reference.generate()
    expression.index.generate()
  }

}

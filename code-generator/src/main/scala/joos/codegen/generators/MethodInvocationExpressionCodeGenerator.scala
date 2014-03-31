package joos.codegen.generators

import joos.ast.expressions.MethodInvocationExpression
import joos.codegen.AssemblyCodeGeneratorEnvironment

class MethodInvocationExpressionCodeGenerator(invocation: MethodInvocationExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment)
    extends AssemblyCodeGenerator {

  override def generate() {
    invocation.expression.foreach(_.generate())
    invocation.arguments.foreach(_.generate())
  }
}

package joos.codegen.generators
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.expressions.InstanceOfExpression
import joos.codegen.AssemblyFileManager

class InstanceOfExpressionCodeGenerator(expression: InstanceOfExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    expression.expression.generate()
  }

}

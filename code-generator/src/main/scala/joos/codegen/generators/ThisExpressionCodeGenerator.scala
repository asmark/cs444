package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.ThisExpression
import joos.codegen.AssemblyCodeGeneratorEnvironment

class ThisExpressionCodeGenerator(expression: ThisExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    appendText(
      mov(Eax, Ecx) :# "Move this into eax"
    )
  }

}

package joos.codegen.generators

import joos.assemgen._
import joos.assemgen.Register._
import joos.ast.expressions.NullLiteral
import joos.codegen.AssemblyCodeGeneratorEnvironment

class NullLiteralCodeGenerator(expression: NullLiteral)
     (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    appendText(
      mov(Eax, 0) :# "Move null into eax"
    )
  }

}

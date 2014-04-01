package joos.codegen.generators


import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.IntegerLiteral
import joos.codegen.AssemblyCodeGeneratorEnvironment

class IntegerLiteralCodeGenerator(literal: IntegerLiteral)
    (implicit val environment: AssemblyCodeGeneratorEnvironment)
    extends AssemblyCodeGenerator {

  override def generate() {
    appendText(
      mov (Eax, literal.value) #: s"Assemble integer literal: ${literal}",
      emptyLine
    )
  }
}

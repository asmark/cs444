package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.StringLiteral
import joos.codegen.AssemblyCodeGeneratorEnvironment

class StringLiteralCodeGenerator(literal: StringLiteral)
    (implicit val environment: AssemblyCodeGeneratorEnvironment)
    extends AssemblyCodeGenerator {

  override def generate() {
    appendText(
      emptyLine,
      :# (s"[BEGIN] Assemble string literal: ${literal}"),
      literal.uniqueName :: db(literal.text),
      mov(Eax, labelReference(literal.uniqueName)),
      :# (s"[END] Assemble string literal: ${literal}"),
      emptyLine
    )
  }
}

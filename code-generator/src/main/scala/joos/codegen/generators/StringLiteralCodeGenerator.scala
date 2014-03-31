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
      emptyLine(),
      comment(s"[BEGIN] Assemble string literal: ${literal}"),
      inlineLabel(literal.uniqueName, db(literal.text)),
      mov(Eax, labelReference(literal.uniqueName)),
      comment(s"[END] Assemble string literal: ${literal}"),
      emptyLine()
    )
  }
}

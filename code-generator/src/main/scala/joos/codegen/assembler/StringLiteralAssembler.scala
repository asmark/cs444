package joos.codegen.assembler

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.StringLiteral
import joos.codegen.AssemblyFileManager

class StringLiteralAssembler(literal: StringLiteral)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {

  override def generateAssembly() {
    Seq(
      emptyLine(),
      comment(s"[BEGIN] Assemble string literal: ${literal}"),
      inlineLabel(literal.uniqueName, db(literal.text)),
      mov(Eax, labelReference(literal.uniqueName)),
      comment(s"[END] Assemble string literal: ${literal}"),
      emptyLine()
    ) foreach assemblyManager.appendText
  }
}

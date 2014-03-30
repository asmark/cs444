package joos.codegen.assembler

import joos.ast.expressions.StringLiteral
import joos.codegen.AssemblyFileManager

class StringLiteralAssembler(literal: StringLiteral)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {

  override def generateAssembly() {
    Seq(
    )
  }

}

package joos.codegen.assembler

import joos.ast.expressions.StringLiteral
import joos.codegen.AssemblyFileManager
import joos.assemgen._
import joos.assemgen.Register._

class StringLiteralAssembler(literal: StringLiteral)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {

  override def generateAssembly() {
    assemblyManager.text =
        // TODO: Can't be done until we have unique ids on strings
        assemblyManager.text
//            label(literal.uniqueId) +: db(literal.toString) :+
//            mov(Eax, labelReference(literal.uniqueId), s"Assemble integer literal: ${literal.value}")
  }

}

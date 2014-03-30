package joos.codegen.assembler

import joos.ast.statements.Block
import joos.codegen.AssemblyFileManager

class BlockAssembler(val block: Block)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

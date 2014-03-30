package joos.codegen.assembler

import joos.ast.declarations.VariableDeclarationFragment
import joos.codegen.AssemblyFileManager

class VariableDeclarationFragmentAssembler(expression: VariableDeclarationFragment)(implicit val assemblyManager: AssemblyFileManager) extends Assembler{
  override def generateAssembly(): Unit = {}
}

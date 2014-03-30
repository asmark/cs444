package joos.codegen.assembler

import joos.ast.declarations.SingleVariableDeclaration
import joos.codegen.AssemblyFileManager

class SingleVarDeclarationAssembler(variable: SingleVariableDeclaration)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

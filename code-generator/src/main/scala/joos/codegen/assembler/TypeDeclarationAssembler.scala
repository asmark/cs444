package joos.codegen.assembler

import joos.ast.declarations.TypeDeclaration
import joos.codegen.AssemblyFileManager

class TypeDeclarationAssembler(typed: TypeDeclaration)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

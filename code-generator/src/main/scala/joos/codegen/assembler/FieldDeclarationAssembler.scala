package joos.codegen.assembler

import joos.ast.declarations.FieldDeclaration
import joos.codegen.AssemblyFileManager

class FieldDeclarationAssembler(field: FieldDeclaration)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {

  }
}

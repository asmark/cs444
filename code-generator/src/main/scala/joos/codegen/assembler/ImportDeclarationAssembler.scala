package joos.codegen.assembler

import joos.ast.declarations.ImportDeclaration
import joos.codegen.AssemblyFileManager

class ImportDeclarationAssembler(importDeclaration: ImportDeclaration)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

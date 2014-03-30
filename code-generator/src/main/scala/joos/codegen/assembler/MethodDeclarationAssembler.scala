package joos.codegen.assembler

import joos.ast.declarations.MethodDeclaration
import joos.codegen.AssemblyFileManager

class MethodDeclarationAssembler(val methodDeclaration: MethodDeclaration)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

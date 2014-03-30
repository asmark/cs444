package joos.codegen.assembler

import joos.ast.declarations.TypeDeclaration
import joos.codegen.AssemblyFileManager
import joos.assemgen._

class TypeDeclarationAssembler(typed: TypeDeclaration)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {
    assemblyManager.globals.add(global(LabelReference(typed.uniqueName)))

    assemblyManager.text = assemblyManager.text :+ label(typed.uniqueName)
    typed.containedMethods.values.flatten.foreach(
      methodDecl => assemblyManager.text = assemblyManager.text :+ dd(LabelReference(methodDecl.uniqueName))
    )

    assemblyManager.text = assemblyManager.text :+ emptyLine()
  }
}

package joos.codegen.assembler

import joos.assemgen._
import joos.ast.declarations.TypeDeclaration
import joos.codegen.AssemblyFileManager

class TypeDeclarationAssembler(typed: TypeDeclaration)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {
    assemblyManager.appendGlobal(global(LabelReference(typed.uniqueName)))

    assemblyManager.appendText(label(typed.uniqueName))
    typed.containedMethods.values.flatten.foreach(
      methodDeclaration =>
        assemblyManager.appendText(dd(labelReference(methodDeclaration.uniqueName)))
    )

    assemblyManager.appendText(emptyLine())
  }
}

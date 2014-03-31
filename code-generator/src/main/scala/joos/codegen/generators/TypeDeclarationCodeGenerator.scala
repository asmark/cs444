package joos.codegen.generators

import joos.assemgen._
import joos.ast.declarations.TypeDeclaration
import joos.codegen.AssemblyCodeGeneratorEnvironment

class TypeDeclarationCodeGenerator(tipe: TypeDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {

    appendGlobal(
      global(LabelReference(tipe.uniqueName))
    )

    appendText(
      label(tipe.uniqueName)

    )

    tipe.containedMethods.values.flatten.foreach(
      methodDeclaration =>
        appendText(dd(labelReference(methodDeclaration.uniqueName)))
    )

    appendText(emptyLine())
  }
}

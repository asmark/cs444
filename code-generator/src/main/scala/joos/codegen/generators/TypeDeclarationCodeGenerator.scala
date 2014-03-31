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

    appendText(comment("Declaring class"))
    appendText(
          label(tipe.uniqueName)
    )

    // Add methods to class definition
    val methods = tipe.containedMethods.values.flatten
    methods.foreach {
      methodDeclaration =>
        appendText(dd(labelReference(methodDeclaration.uniqueName)))
    }
    appendText(emptyLine())

    methods.foreach(_.generate())
    appendText(emptyLine())
  }
}

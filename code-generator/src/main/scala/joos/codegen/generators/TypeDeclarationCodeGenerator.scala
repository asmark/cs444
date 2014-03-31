package joos.codegen.generators

import joos.assemgen._
import joos.ast.declarations.TypeDeclaration
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.Modifier

class TypeDeclarationCodeGenerator(tipe: TypeDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {

    appendGlobal(tipe.uniqueName)

    appendText(comment("Declaring class"))
    appendText(
          label(tipe.uniqueName)
    )

    // Add static fields
    val staticFields = tipe.containedFields.values.filter(
      field => {
        if (field.isStatic)
          true
        false
      }
    )
    val indexedFields = staticFields.toIndexedSeq
    for (i <- 0 until indexedFields.size) {
      appendText(dd(0))
      appendData(inlineLabel(indexedFields(i).uniqueName + offsetPostFix, dd(i * 4)))
    }

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

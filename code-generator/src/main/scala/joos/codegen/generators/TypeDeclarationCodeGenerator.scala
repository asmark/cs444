package joos.codegen.generators

import joos.assemgen._
import joos.ast.declarations.{FieldDeclaration, TypeDeclaration}
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.Modifier
import scala.collection.mutable
import joos.ast.expressions.SimpleNameExpression

class TypeDeclarationCodeGenerator(tipe: TypeDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {

    appendGlobal(tipe.uniqueName)

    appendText(#: ("Declaring class"))
    appendText(tipe.uniqueName::)

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
      appendData(indexedFields(i).uniqueName + offsetPostFix :: dd(i * 4))
    }

    // Add methods to class definition
    val containedMethods = tipe.containedMethods.values.flatten
    containedMethods.foreach {
      methodDeclaration =>
        appendText(dd(labelReference(methodDeclaration.uniqueName)))

    }
    appendText(emptyLine)

    tipe.methodMap.values.foreach(_.generate())
    appendText(emptyLine)
  }
}

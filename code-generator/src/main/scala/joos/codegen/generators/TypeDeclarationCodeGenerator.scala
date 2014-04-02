package joos.codegen.generators

import joos.assemgen._
import joos.ast.declarations.TypeDeclaration
import joos.codegen.AssemblyCodeGeneratorEnvironment

class TypeDeclarationCodeGenerator(tipe: TypeDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  private final val FieldOffset = 8

  override def generate() {
    tipe.methodMap.values.foreach(_.generate())
    appendText(emptyLine)

    tipe.constructorMap.values.foreach(_.generate())
    appendText(emptyLine)

    // Add malloc method
    generateTables()
    generateMallocMethods()
  }

  private def generateTables() {
    val objectInfoTable = nextLabel(s"object_info_${tipe.uniqueName}")
    val selectorTable = nextLabel(s"selector_table_${tipe.uniqueName}")
    val subtypeTable = nextLabel(s"subtype_table_${tipe.uniqueName}")

    appendGlobal(objectInfoTable)

    appendText(
      objectInfoTable ::,
      dd(selectorTable),
      dd(subtypeTable)
    )

    var index = 0
    tipe.containedFields.foreach {
      entry =>
        appendText(entry._2.uniqueName :: dd(FieldOffset + index*4))
        index += 1
    }
    appendText(emptyLine)

    // TODO: Generate selector table
    appendGlobal(selectorTable)
    appendText(selectorTable ::, emptyLine)

    // TODO: Generate subtype table
    appendGlobal(subtypeTable)
    appendText(subtypeTable ::, emptyLine)



    // TODO: generate array class info tables
  }

  private def generateMallocMethods() {

    // TODO: Generate array malloc
  }
}

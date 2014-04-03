package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.declarations.TypeDeclaration
import joos.codegen.AssemblyCodeGeneratorEnvironment

class TypeDeclarationCodeGenerator(tipe: TypeDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  val selectorTable = selectorTableLabel(tipe)
  val subtypeTable = subtypeTableLabel(tipe)

  override def generate() {

    environment.typeEnvironment = tipe

    tipe.methodMap.values.foreach(_.generate())
    appendText(emptyLine)

    tipe.constructorMap.values.foreach(_.generate())
    appendText(emptyLine)

    // Add malloc method
    generateTables()
    generateMallocMethods()
  }

  private def generateTables() {
    appendData(:#(s"[BEGIN] Storage location for all static members for ${tipe.fullName}"))
    tipe.fieldMap.values.filter(_.isStatic).foreach {
      field =>
        appendGlobal(field.uniqueName)
        appendData((field.uniqueName :: dd(0)) :# s"Storage location for static ${field.typeDeclaration.fullName}.${field.declarationName}")
    }
    appendData(
      :#(s"[END] Storage location for all static members for ${tipe.fullName}"),
      emptyLine
    )

    createSelectorIndexedTable()

    // TODO: Generate subtype table
    appendGlobal(subtypeTable)
    appendText(subtypeTable ::, emptyLine)



    // TODO: generate array class info tables
  }

  private def generateMallocMethods() {
    val mallocThis = mallocTypeLabel(tipe)
    appendGlobal(mallocThis)

    appendText(mallocThis ::)
    appendText(prologue(0): _*)

    appendText(
      mov(Eax, FieldOffset + tipe.objectSize) :# s"Allocate ${8 + tipe.objectSize} bytes for object",
      call(mallocLabel),
      movdw(at(Eax), selectorTable) :# "Bind selector table",
      movdw(at(Eax + 4), subtypeTable) :# "Bind subtype table"
    )
    tipe.instanceFields.foreach {
      field =>
        val offset = tipe.getFieldSlot(field.declarationName) * 4 + FieldOffset
        appendText(movdw(at(Eax + offset), 0) :# s"Initialize ${field.declarationName} to default value")
    }
    appendText(
      :#("[END] Constructor Default Initialization"),
      emptyLine,
      mov(Ecx, Eax) :# "Move this into ecx"
    )

    appendText(epilogue: _*)
    // TODO: Null check?

    // TODO: Generate array malloc
  }

  private def createSelectorIndexedTable() {
    appendGlobal(selectorTable)
    appendData(selectorTable ::, emptyLine)
    environment.sitManager.orderedMethods.foreach(
      method => {
        if (tipe.methodMap.values.toSet.contains(method)) {
          appendData(dd(labelReference(method.uniqueName)) :#method.uniqueName)
        } else {
          appendData(dd(0) :#method.uniqueName)
        }
      }
    )
  }
}

package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.declarations.TypeDeclaration
import joos.codegen.AssemblyCodeGeneratorEnvironment

class TypeDeclarationCodeGenerator(tipe: TypeDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  val objectInfoTable = objectInfoTableLabel(tipe)
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

  def createSubtypeTable() = {
    appendGlobal(subtypeTable)
    appendData(subtypeTable ::, emptyLine)

    environment.staticDataManager.orderedTypes.foreach(
      target => {
        if (target.allAncestors.map(x => x.uniqueName).contains(tipe.uniqueName)) {
          appendData(dd(1) :#target.uniqueName)
        } else {
          if (target.uniqueName equals tipe.uniqueName) {
            appendData(dd(1) :#target.uniqueName)
          } else {
            appendData(dd(0) :#target.uniqueName)
          }
        }
      }
    )

    appendData(emptyLine)
  }

  private def generateTables() {
    appendGlobal(objectInfoTable)

    appendText(
      objectInfoTable ::,
      dd(selectorTable),
      dd(subtypeTable),
      emptyLine
    )

    createSelectorIndexedTable()

    createSubtypeTable()


    // TODO: generate array class info tables
  }

  private def generateMallocMethods() {
    val mallocThis = mallocTypeLabel(tipe)
    appendGlobal(mallocThis)

    appendText(
      mallocThis ::,
      mov(Eax, FieldOffset + tipe.objectSize) :# s"Allocate ${8 + tipe.objectSize} bytes for object",
      call(mallocLabel),
      movdw(at(Eax), selectorTable) :# "Bind selector table",
      movdw(at(Eax + 4), subtypeTable) :# "Bind subtype table",
      mov(Ecx, Eax) :# "Move this into ecx"
    )
    // TODO: Initialize fields
    // TODO: Null check?

    appendText(ret)

    // TODO: Generate array malloc
  }

  private def createSelectorIndexedTable() {
    appendGlobal(selectorTable)
    appendData(selectorTable ::, emptyLine)
    environment.staticDataManager.orderedMethods.foreach(
      method => {
        if (tipe.methods.map(x => x.uniqueName).toSet.contains(method.uniqueName)) {
          appendData(dd(labelReference(method.uniqueName)) :#method.uniqueName)
        } else {
          appendData(dd(0) :#method.uniqueName)
        }
      }
    )
    appendData(emptyLine)
  }
}

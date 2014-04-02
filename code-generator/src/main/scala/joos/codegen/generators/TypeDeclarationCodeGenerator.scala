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

  private def generateTables() {
    appendGlobal(objectInfoTable)

    appendText(
      objectInfoTable ::,
      dd(selectorTable),
      dd(subtypeTable),
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
    environment.sitManager.orderedMethods.foreach(
      method => {
        if (tipe.methodMap.values.toSet.contains(method)) {
          appendData(dd(labelReference(method.uniqueName)))
        } else {
          appendData(toExpression(0))
        }
      }
    )
  }
}

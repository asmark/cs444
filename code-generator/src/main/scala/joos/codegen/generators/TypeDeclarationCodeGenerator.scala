package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.declarations.{MethodDeclaration, TypeDeclaration}
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.semantic._
import joos.ast.types.PrimitiveType

class TypeDeclarationCodeGenerator(tipe: TypeDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  val selectorTable = selectorTableLabel(tipe)
  val subtypeTable = subtypeTableLabel(tipe)
  val classTable = classTableLabel(tipe)

  override def generate() {

    environment.typeEnvironment = tipe

    tipe.methodMap.values.foreach(_.generate())
    appendText(emptyLine)

    tipe.constructorMap.values.foreach(_.generate())
    appendText(emptyLine)

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
    createSelectorIndexedTableForArray()

    createSubtypeTable()
    createSubtypeTableForArray()
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
  }

  private def createSelectorIndexedTable() {

    appendGlobal(classTable)
    appendData(classTable ::, dd(selectorTable), emptyLine)

    def includeOverridden(methods: Traversable[MethodDeclaration]): Map[MethodDeclaration, MethodDeclaration] = {

      def getOverridden(method: MethodDeclaration, implementer: MethodDeclaration): Set[MethodDeclaration] = {
        Set(method) ++ (method.overloads match {
          case Some(overloaded) =>
            getOverridden(overloaded, implementer)
          case None => Set.empty
        })
      }

      methods.foldRight(Map.empty[MethodDeclaration, MethodDeclaration]) {
        (implementer, implementerMap) =>
          implementerMap ++ getOverridden(implementer, implementer).map(_ -> implementer)
      }
    }

    appendGlobal(selectorTable)
    appendData(selectorTable ::, emptyLine)
    val containedMethods = tipe.implementedMethods.values.flatten
    val supportedMethods = includeOverridden(containedMethods)

    environment.staticDataManager.orderedMethods.foreach {
      method =>
        supportedMethods.get(method) match {
          case Some(implementer) => {
            appendData(dd(implementer.uniqueName) :# s"${method.uniqueName} implemented by ${implementer.uniqueName}")
          }
          case None => appendData(dd(0) :# s"${method.uniqueName} not implemented by ${tipe.uniqueName}")
        }
    }

    appendData(emptyLine)
  }

  private def createSelectorIndexedTableForArray(): Unit = {
    appendGlobal(arrayPrefixLabel(selectorTable))
    appendData(arrayPrefixLabel(selectorTable) ::, emptyLine)

    // Arrays only have the java.lang.Object methods
    environment.staticDataManager.orderedMethods.foreach {
      method =>
        if (isArraySuperType(method.typeDeclaration)) {
          appendData(dd(method.uniqueName) :# s"${method.uniqueName} implemented by ${method.uniqueName}")
        } else {
          appendData(dd(0) :# s"${method.uniqueName} not implemented by ${arrayPrefixLabel(tipe.uniqueName)}")
        }
    }
    appendData(emptyLine)
  }

  private def createSubtypeTable() {
    appendGlobal(subtypeTable)
    appendData(subtypeTable ::, emptyLine)

    environment.staticDataManager.orderedTypes.foreach {
      target =>
        if (tipe.allAncestors.map(ancesotr => ancesotr.uniqueName).contains(target.uniqueName) || (tipe.fullName equals target.fullName)) {
          appendData(dd(1) :# target.fullName)
        } else {
          appendData(dd(0) :# target.fullName)
        }
    }

    environment.staticDataManager.orderedTypes.foreach {
      target =>
        if (tipe.allAncestors.map(ancestor => ancestor.uniqueName).contains(target.uniqueName) || (tipe.fullName equals target.fullName)) {
          appendData(dd(0) :# arrayPrefixLabel(target.fullName))
        } else {
          appendData(dd(0) :# arrayPrefixLabel(target.fullName))
        }
    }

    appendData(emptyLine)
  }

  private def createSubtypeTableForArray(): Unit = {
    appendGlobal(arrayPrefixLabel(subtypeTable))
    appendData(arrayPrefixLabel(subtypeTable) ::, emptyLine)

    // Handle normal types first. Arrays are not subtype of any objects but Object Serializable and Cloneable
    environment.staticDataManager.orderedTypes.foreach {
      target =>
        if (isArraySuperType(target)) {
          appendData(dd(1) :# target.fullName)
        } else {
          appendData(dd(0) :# target.fullName)
        }
    }

    // Handle other array types. Arrays are covariant
    environment.staticDataManager.orderedTypes.foreach {
      target =>
        if (tipe.allAncestors.map(ancestor => ancestor.uniqueName).contains(target.uniqueName) ||
            (tipe.fullName equals target.fullName) ||
            isArraySuperType(tipe)) {
          appendData(dd(1) :# arrayPrefixLabel(target.fullName))
        } else {
          appendData(dd(0) :# arrayPrefixLabel(target.fullName))
        }
    }

    // Append primitive array types
    PrimitiveType.values.foreach {
      primitive =>
        appendData(dd(0) :# arrayPrefixLabel(primitive.uniqueName))
    }

    appendData(emptyLine)
  }

}

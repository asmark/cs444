package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.declarations.MethodDeclaration
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.semantic.getSuperType

class MethodDeclarationCodeGenerator(method: MethodDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    if (method.isNative) {
      return
    }

    environment.methodEnvironment = method

    method.isConstructor match {
      case true => generateConstructorCode()
      case false => generateMethodCode()
    }
  }

  def generateConstructorCode() {
    val constructorLabel = s"${method.uniqueName}"
    appendGlobal(constructorLabel)
    appendText(
      :#("[BEGIN] Constructor Definition"),
      constructorLabel ::
    )

    appendText(prologue(4 * method.locals): _*)

    // Expect eax to hold pointer to raw malloc'ed object
    getSuperType(environment.typeEnvironment) match {
      case None => {
        appendText(:#("Object has no super constructor. Not invoking super constructor"))
      }
      case Some(superType) => {
        val superConstructor = superType.constructorMap.values.find(constructor => constructor.parameters.length == 0).get
        appendText(
          call(superConstructor.uniqueName) :# "Invoke super constructor. Returns pointer in ecx"
        )
      }
    }

    // Pointer should still be in Ecx
    appendText(
      push(Ecx) :# "Preserve this"
    )

    val tipe = environment.typeEnvironment
    appendText(:#("[BEGIN] Constructor Initialization"))
    tipe.instanceFields.foreach {
      field =>
          if (field.typeDeclaration equals tipe) {
            field.generate()
          }
    }
    appendText(:#("[END] Constructor Initialization"), emptyLine)

    appendText(:#("[BEGIN] Constructor Body"), #>)
    method.body.foreach(_.generate())
    appendText(#<, :#("[END] Constructor Body"), emptyLine)

    appendText(
      pop(Ecx) :# "Retrieve this"
    )
    appendText(epilogue: _*)

    appendText(
      :#("[END] Constructor Definition"),
      emptyLine
    )
  }

  def generateMethodCode() {
    if (method.name.standardName == "test") {
      generateStartCode()
    }

    val methodLabel = s"${method.uniqueName}"
    appendGlobal(methodLabel)

    appendText(
      :#("[BEGIN] Method Definition"),
      methodLabel ::
    )

    appendText(prologue(4 * method.locals): _*)

    appendText(:#("[BEGIN] Function Body"), #>)
    method.body.foreach(_.generate())
    appendText(#<, :#("[END] Function Body"), emptyLine)

    appendText(epilogue: _*)

    appendText(
      :#("[END] Method Definition"),
      emptyLine
    )
  }

  def generateStartCode() {

    val tipe = environment.typeEnvironment
    val startLabel = "_start"

    appendGlobal(startLabel)

    appendText(
      startLabel ::,
      :#("[BEGIN] Static field initializations"),
      :#(s"Initialize statics of ${tipe.fullName}"),
      #>
    )
    // Do not use tipe.staticFields since this pulls in inherited ones as well
    tipe.fieldMap.values.withFilter(_.isStatic).foreach(_.generate())

    appendText(#<)

    tipe.compilationUnit.moduleDeclaration.namespace.getAllTypes(Set(tipe)).foreach {
      typeDeclaration =>
        appendText(:#(s"Initialize statics of ${typeDeclaration.fullName}"), #>)
        typeDeclaration.fieldMap.values.withFilter(_.isStatic).foreach(_.generate())
        appendText(#<)
    }

    appendText(
      #<,
      :#("[END] Static field initializations"),
      emptyLine,
      call(labelReference(method.uniqueName)),
      emptyLine,
      mov(Ebx, Eax),
      mov(Eax, 1),
      int(0x80),
      emptyLine
    )

  }
}

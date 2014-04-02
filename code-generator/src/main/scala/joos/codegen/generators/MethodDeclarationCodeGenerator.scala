package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.Modifier
import joos.ast.declarations.MethodDeclaration
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.semantic.getSuperType

class MethodDeclarationCodeGenerator(method: MethodDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {

    environment.resetVariables()
    environment.numLocals = method.locals
    method.parameters.foreach {
      parameter =>
        environment.addParameterSlot(parameter.declarationName)
    }

    if (method.modifiers contains Modifier.Native) {
      // TODO: Not sure what to do here?
      return
    }

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

    appendText(prologue(4 * environment.numLocals): _*)

    // Expect eax to hold pointer to raw malloc'ed object
    getSuperType(method.typeDeclaration) match {
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

    appendText(:#("[BEGIN] Constructor Default Initialization"), #>)
    // TODO: Initializations
    appendText(#<, :#("[END] Constructor Default Initialization"), emptyLine)

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

    appendText(prologue(4 * environment.numLocals): _*)

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
    val startLabel = "_start"

    appendGlobal(startLabel)

    appendText(
      startLabel ::,
      :#("[BEGIN] Static field initializations"),
      // TODO: Initializations
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

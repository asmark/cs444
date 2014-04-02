package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.declarations.MethodDeclaration
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.Modifier

class MethodDeclarationCodeGenerator(method: MethodDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  // TODO: Constructors should return "this"
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

    if (method.name.standardName == "test") {
      generateStartCode()
    }

    val methodLabel = s"${method.uniqueName}"
    appendGlobal(methodLabel)

    appendText(
      #: ("[BEGIN] Method Definition"),
      methodLabel::
    )

    appendText(prologue(4 * environment.numLocals): _*)

    appendText(#: ("[BEGIN] Function Body"), #>)
    method.body.foreach(_.generate())
    appendText(#<, #: ("[END] Function Body"), emptyLine)

    if (method.isConstructor) {
      // TODO: Return this as Eax
    }

    appendText(epilogue: _*)

    appendText(
      #: ("[END] Method Definition"),
      emptyLine
    )

  }

  def generateStartCode() {
    val startLabel = "_start"

    appendGlobal(startLabel)

    appendText(
      startLabel::,
      #: ("[BEGIN] Static field initializations"),
      // TODO: Initializations
      #: ("[END] Static field initializations"),
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

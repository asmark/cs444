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
    appendText(prologue(0): _*)

    appendText(#: ("[BEGIN] Function Body"), #>)
    method.body.foreach(_.generate())
    appendText(#<, #: ("[BEGIN] Function End"), emptyLine)

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

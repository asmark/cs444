package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.declarations.MethodDeclaration
import joos.codegen.AssemblyCodeGeneratorEnvironment

class MethodDeclarationCodeGenerator(method: MethodDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  // TODO: Constructors should return "this"
  override def generate() {

    if (method.name.standardName == "test") {
      generateStartCode
    }

    val methodLabel = s"${method.uniqueName}"
    appendGlobal(methodLabel)

    appendText(
      #: ("[BEGIN] Method Definition"),
      label(methodLabel)
    )
    appendText(prologue(4): _*)

    appendText(#: ("[BEGIN] Function Body"))
    method.body.foreach(_.generate())
    appendText(#: ("[BEGIN] Function End"), emptyLine())

    appendText(epilogue: _*)

    appendText(
      #: ("[END] Method Definition"),
      emptyLine()
    )

  }

  def generateStartCode {
    val startLabel = "_start"

    appendGlobal(startLabel)

    appendText(
      label(startLabel),
      #: ("[BEGIN] Static field initializations"),
      // TODO: Initializations
      #: ("[END] Static field initializations"),
      emptyLine(),
      call(labelReference(method.uniqueName)),
      emptyLine(),
      mov(Ebx, Eax),
      mov(Eax, 1),
      int(0x80),
      emptyLine()
    )

  }
}

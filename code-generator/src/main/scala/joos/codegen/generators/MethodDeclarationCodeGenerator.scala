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
      comment("[BEGIN] Method Definition"),
      label(methodLabel)
    )
    appendText(prologue(4): _*)

    appendText(comment("[BEGIN] Function Body"))
    method.body.foreach(_.generate())
    appendText(comment("[BEGIN] Function End"), emptyLine())

    appendText(epilogue: _*)

    appendText(
      comment("[END] Method Definition"),
      emptyLine()
    )

  }

  def generateStartCode {
    val startLabel = "_start"

    appendGlobal(startLabel)

    appendText(
      label(startLabel),
      comment("[BEGIN] Static field initializations"),
      // TODO: Initializations
      comment("[END] Static field initializations"),
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

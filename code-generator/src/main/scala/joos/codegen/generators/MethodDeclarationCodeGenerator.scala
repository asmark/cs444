package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.declarations.MethodDeclaration
import joos.codegen.AssemblyCodeGeneratorEnvironment

class MethodDeclarationCodeGenerator(method: MethodDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  def prologue = Seq(
    comment("[BEGIN] Function Prologue"),
    push(Ebp),
    mov(Ebp, Esp),
    sub(Esp, 4 /* TODO */),
    push(Ebx),
    push(Ecx),
    push(Edx),
    push(Esi),
    push(Edi),
    comment("[END] Function Prologue"),
    emptyLine()
  )

  def epilogue = Seq(
    comment("[BEGIN] Function Epilogue"),
    pop(Edi),
    pop(Esi),
    pop(Edx),
    pop(Ecx),
    pop(Ebx),
    mov(Esp, Ebp),
    pop(Ebp),
    ret,
    comment("[END] Function Epilogue")
  )

  // TODO: Constructors should return "this"
  override def generate() {

    if (method.name.standardName == "test") {
      generateStartCode
    }

    val methodLabel = s"${method.uniqueName}"
    appendGlobal(global(methodLabel))

    appendText(
      comment("[BEGIN] Method Definition"),
      label(methodLabel)
    )
    appendText(prologue: _*)

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

    appendGlobal(global(startLabel))

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

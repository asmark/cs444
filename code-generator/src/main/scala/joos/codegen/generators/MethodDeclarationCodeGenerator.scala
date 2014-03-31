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
    sub(Ebp, 4),
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
    val methodLabel = s"${method.uniqueName}"
    appendGlobal(global(methodLabel))

    appendText(
      comment("[BEGIN] Method Definition"),
      label(methodLabel)
    )
    appendText(prologue: _*)

    method.body.foreach(_.generate())

    appendText(epilogue: _*)

    appendText(
      comment("[END] Method Definition"),
      emptyLine()
    )

  }
}

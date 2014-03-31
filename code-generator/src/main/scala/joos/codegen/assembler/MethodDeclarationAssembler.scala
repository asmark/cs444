package joos.codegen.assembler

import joos.ast.declarations.MethodDeclaration
import joos.codegen.AssemblyFileManager
import joos.assemgen._
import joos.assemgen.Register._


class MethodDeclarationAssembler(val methodDeclaration: MethodDeclaration)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly() {
    // Placeholder tester:
    if (methodDeclaration.name.standardName == "test") {
      assemblyManager.appendGlobal(global(LabelReference("_start")))
      assemblyManager.appendText(label("_start"))
      assemblyManager.appendText(
        call(LabelReference(methodDeclaration.uniqueName)),
        mov(Eax, at(Eax)),
        mov(Ebx, Eax),
        mov(Eax, 1),
        int(0x80),
        emptyLine()
      )
    }


    assemblyManager.appendGlobal(global(LabelReference(methodDeclaration.uniqueName)))
    assemblyManager.appendText(
      label(methodDeclaration.uniqueName),
      comment("[BEGIN] Function Prologue"),
      push(Ebp),
      mov(Ebp, Esp),
      sub(Esp, 4),
      push(Ebx),
      push(Ecx),
      push(Edx),
      push(Esi),
      push(Edi),
      comment("[END] Function Prologue")
    )

  }
}

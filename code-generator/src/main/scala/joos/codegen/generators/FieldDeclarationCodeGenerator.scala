package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.declarations.FieldDeclaration

class FieldDeclarationCodeGenerator(field: FieldDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    val tipe = environment.typeEnvironment
    val offset = tipe.getFieldSlot(field.declarationName) * 4 + FieldOffset

    appendText(:#(s"[BEGIN] Initializing ${field}"))

    val rhs = field.fragment.initializer
    if (rhs.isDefined) {
      appendText(
        push(Ecx) :# "Save this",
        #>,
        :#("Evaluate right hand side")
      )

      rhs.get.generate()
      // Rhs is in Eax now
      appendText(
        #<,
        pop(Ecx) :# "Restore this",
        movdw(at(Ecx + offset), Eax) :# s"Assign ${field}",
        :#("[END] Initializing field: " + field.declarationName)
      )
    }
  }

}

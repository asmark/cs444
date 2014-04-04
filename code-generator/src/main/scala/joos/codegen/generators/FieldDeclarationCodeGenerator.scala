package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.declarations.FieldDeclaration
import joos.codegen.AssemblyCodeGeneratorEnvironment

class FieldDeclarationCodeGenerator(field: FieldDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    field.isStatic match {
      case true => initializeStaticField()
      case false => initializeInstanceField()
    }
  }

  def initializeStaticField() {
    val tipe = field.typeDeclaration

    appendText(:#(s"[BEGIN] Initialize static field ${tipe.fullName}.${field.declarationName}"))

    val rhs = field.fragment.initializer
    if (rhs.isDefined) {
      appendText(
        push(Ecx) :# "Save this (probably not necessary)",
        #>,
        :#("Evaluate right hand side")
      )

      rhs.get.generate()
      // Rhs is in Eax now
      appendText(
        #<,
        pop(Ecx) :# "Restore this (probably not necessary)",
        movdw(Edx, field.uniqueName) :# s"Move field storage location into edx",
        movdw(at(Edx), Eax) :# s"Assign ${tipe.fullName}.${field.declarationName}",
        :#(s"[END] Initialize static field ${tipe.fullName}.${field.declarationName}")
      )
    }

  }

  def initializeInstanceField() {
    val tipe = field.typeDeclaration
    val offset = tipe.getFieldSlot(field.declarationName) * 4 + FieldOffset

    appendText(:#(s"[BEGIN] Initializing instance field ${field.declarationName}"))

    val rhs = field.fragment.initializer
    rhs match {
      case Some(initializer) => {
        appendText(
          push(Ecx) :# "Save this",
          #>,
          :#("Evaluate right hand side")
        )

        initializer.generate()
        // Rhs is in Eax now
        appendText(
          #<,
          pop(Ecx) :# "Restore this"
        )
      }
      case None => appendText(mov(Eax, 0) :# s"${field.declarationName} has no initializer")
    }

    appendText(
      movdw(at(Ecx + offset), Eax) :# s"Assign ${field}",
      :#(s"[END] Initializing instance field ${field.declarationName}")
    )
  }

}

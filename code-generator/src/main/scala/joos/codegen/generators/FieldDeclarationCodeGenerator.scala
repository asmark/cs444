package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.declarations.FieldDeclaration

class FieldDeclarationCodeGenerator(field: FieldDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    field.declarationName.generate()
    val offset = field.typeDeclaration.getFieldSlot(field.declarationName) * 4;

    appendText(
      :#("[BEGIN] Initializing field: " + field.declarationName.standardName),
      #>
    )
    prologue(0)

    val rhs = field.fragment.initializer

    rhs match {
      case Some(initializer) => {
        appendText(:#("Calling initializer"))
        initializer.generate()
        // EAX should hold the return value
        appendText(
          add(Edx, offset),
          mov(Edx, Eax)
        )
      }
      case None => {
        appendText(:#("Assigning default value"))
        appendText(
          add(Edx, offset),
          mov(Edx, toExpression(0))
        )
      }
    }

    epilogue(0)
    appendText(
      #<,
      :#("[END] Initializing field: " + field.declarationName.standardName)
    )
  }

}

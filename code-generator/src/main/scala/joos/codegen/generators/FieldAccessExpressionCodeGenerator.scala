package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.expressions.FieldAccessExpression

class FieldAccessExpressionCodeGenerator(expression: FieldAccessExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    expression.expression.generate()

    val declaration = expression.declaration
    val offsetLabel = declaration.uniqueName + offsetPostFix
    if (declaration.isStatic) {
      // Assume EAX now has the reference to the type def
      appendText(
        comment(s"[BEG] accessing static field: ${expression.identifier.standardName}"),
        push(Ebx),
        mov(Ebx, at(labelReference(offsetLabel))),
        add(Eax, Ebx),
        pop(Ebx),
        comment(s"[END] accessing static field: ${expression.identifier.standardName}")
      )
    } else {
      // Assume EAX now has the reference to the object
      appendText(
        comment(s"[BEG] accessing instance field: ${expression.identifier.standardName}"),
        push(Ebx),
        // The first field in the object is a link to the class table
        mov(Ebx, at(labelReference(offsetLabel))),
        add(Eax, Ebx),
        pop(Ebx),
        comment(s"[END] accessing instance field: ${expression.identifier.standardName}")
      )
    }
  }

}
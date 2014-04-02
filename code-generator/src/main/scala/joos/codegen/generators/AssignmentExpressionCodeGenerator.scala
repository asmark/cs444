package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.AssignmentExpression
import joos.codegen.AssemblyCodeGeneratorEnvironment

class AssignmentExpressionCodeGenerator(expression: AssignmentExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {

    appendText(
      #:(s"[BEGIN] Assignment Expression ${expression}"),
      emptyLine,
      #:("Find lvalue"),
      #>
    )

    // Should put lvalue in Edx
    expression.left.generate()

    appendText(
      #<,
      push(Edx) :# "Save lvalue",
      emptyLine,
      #:("Find right"),
      #>
    )

    // Should put right in eax
    expression.right.generate()
    appendText(
      #<,
      pop(Ebx) :# "Retrieve lvalue",
      mov(at(Ebx), Eax) :# "Assign lvalue to right",
      emptyLine
    )
  }

}
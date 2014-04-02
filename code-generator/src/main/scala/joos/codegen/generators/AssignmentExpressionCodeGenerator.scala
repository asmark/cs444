package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.{IntegerLiteral, AssignmentExpression}
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.types.{PrimitiveType, SimpleType}

class AssignmentExpressionCodeGenerator(expression: AssignmentExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {

    appendText(
      :#(s"[BEGIN] Assignment Expression ${expression}"),
      push(Ecx) :# "Preserve this",
      emptyLine,
      :#("Find lvalue"),
      #>
    )

    // Should put lvalue in Edx
    expression.left.generate()

    appendText(
      #<,
      push(Edx) :# "Save lvalue",
      emptyLine,
      :#("Find right"),
      #>
    )

    // Should put right in eax
    expression.right.generate()
    appendText(
      #<,
      pop(Ebx) :# "Retrieve lvalue",
      mov(at(Ebx), Eax) :# "Assign lvalue to right",
      pop(Ecx) :# "Retrieve this",
      :#(s"[END] Assignment Expression ${expression}"),
      emptyLine
    )
  }

}
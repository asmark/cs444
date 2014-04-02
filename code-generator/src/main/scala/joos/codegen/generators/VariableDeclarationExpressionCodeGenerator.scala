package joos.codegen.generators

import joos.ast.expressions.VariableDeclarationExpression
import joos.codegen.AssemblyFileManager
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment

class VariableDeclarationExpressionCodeGenerator(expression: VariableDeclarationExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    appendText(
      :#(s"[BEGIN] Variable Initialization for ${expression.declarationName.standardName}"),
      #>
    )
    expression.fragment.generate()
    // Initializer value is now in eax
    val slot = environment.methodEnvironment.getVariableSlot(expression.declarationName)
    assert(slot >= 0)
    appendText(
      mov(Ebx, Ebp),
      sub(Ebx, slot*4) :# "Put the variable into the correct slot above stack",
      mov(at(Ebx), Eax)
    )

    appendText(
      #<,
      :#(s"[END] Variable Initialization for ${expression.declarationName.standardName}"),
      emptyLine
    )

  }

}

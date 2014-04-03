package joos.codegen.generators

import joos.ast.expressions.ArrayAccessExpression
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.BooleanLiteral
import joos.codegen.generators.commonlib._

class ArrayAccessExpressionCodeGenerator(expression: ArrayAccessExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    appendText(
      #>,
      :# ("[BEGIN] Array[]")
    )
    expression.reference.generate()
    // eax has the array reference
    appendText(
      push(Eax) :# "Pass array as argument to null check and save reference",
      call(nullCheck) :# "Check if array reference is null"
    )
    expression.index.generate()
    // eax has the index
    val accessLabel = nextLabel("array.access")
    appendText(
      pop(Ebx) :# "ebx = array reference",
      cmp(Eax, at(Ebx + ArrayLengthOffset)) :# "Array bound check",
      jl(accessLabel),
      call(exceptionLabel),
      accessLabel::,
      imul(Eax, Eax, 4),
      add(Eax, ArrayFirstElementOffset),
      lea(Edx, at(Ebx + Eax)),
      mov(Eax, at(Edx))
    )
    appendText(
      :# ("[END] Array[]"),
      #<
    )
  }

}

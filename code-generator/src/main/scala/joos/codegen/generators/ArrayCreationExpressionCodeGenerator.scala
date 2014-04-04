package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.expressions.ArrayCreationExpression

class ArrayCreationExpressionCodeGenerator(expression: ArrayCreationExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  /**
   * Array object layout
   * 0: SIT table
   * 4: SubType table
   * 8: length
   * 12..: elements
   */
  override def generate() {
    appendText(
      nextLabel("array.creation")::,
      :# ("[BEGIN] Array Creation")
    )
    // Number of elements is in eax
    // TODO: Check Array.length >= 0
    expression.size.generate()
    appendText(
      push(Eax) :# "Saves number of elements",
      add(Eax, 3) :# "Allocates two more fields for SIT and SubType table",
      imul(Eax, Eax, 4),
      call(mallocLabel),
      pop(Ebx),
      mov(at(Eax + ArrayLengthOffset), Ebx) :# "Stores the length at offset 8"
    )

    val loopStart = nextLabel("array.initialization.start")
    val loopEnd = nextLabel("array.initialization.end")
    appendText(
      :# ("[BEGIN] Array Initialization"),
      imul(Ebx, Ebx, 4),
      loopStart::,
      cmp(Ebx, 0),
      je(loopEnd),
      movdw(at(Eax + Ebx + ArrayLengthOffset), 0) :# "Initialize array element to 0",
      sub(Ebx, 4),
      jmp(loopStart),
      loopEnd::,
      :# ("[END] Array Initialization")
    )

    // TODO: binds to SIT and SubType table
    appendText(
      :# ("[END] Array Creation")
    )
  }

}

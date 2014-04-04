package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.expressions.ArrayCreationExpression
import joos.ast.types.SimpleType

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
    val (selectorIndexTable, subtypeTable) = expression.arrayType match {
      case innerType: SimpleType => {
        (new LabelReference(arrayPrefixLabel(selectorTableLabel(innerType.declaration))) ->
        new LabelReference(arrayPrefixLabel(subtypeTableLabel(innerType.declaration))))
      }
      case _ => {
        // TODO: Link primitive tables in later
        (toExpression(0) -> toExpression(0))
      }
    }

    expression.size.generate()
    appendText(
      push(Eax) :# "Saves number of elements",
      add(Eax, 3) :# "Allocates two more fields for SIT and SubType table",
      imul(Eax, Eax, 4),
      call(mallocLabel),
      pop(Ebx),
      movdw(at(Eax + SelectorTableOffset), selectorIndexTable) :# s"Bind selector table at offset ${SelectorTableOffset}",
      movdw(at(Eax + SubtypeTableOffset), subtypeTable) :# s"Bind subtype table at offset ${SubtypeTableOffset}",
      mov(at(Eax + ArrayLengthOffset), Ebx) :# s"Stores the length at offset ${ArrayLengthOffset}"
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

    appendText(
      :# ("[END] Array Creation")
    )
  }

}

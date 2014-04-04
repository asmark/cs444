package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast._
import joos.ast.expressions.{ArrayCreationExpression, StringLiteral}
import joos.ast.types.PrimitiveType._
import joos.ast.types._
import joos.codegen.AssemblyCodeGeneratorEnvironment

class StringLiteralCodeGenerator(literal: StringLiteral)
    (implicit val environment: AssemblyCodeGeneratorEnvironment)
    extends AssemblyCodeGenerator {

  override def generate() {
    appendText(
      emptyLine,
      #>,
      :#(s"[BEGIN] Create String Literal: ${literal}"),
      push(Ecx) :# "Save this"
    )

    ArrayCreationExpression(CharType, literal.text.length).generate()
    for (i <- 0 until literal.text.length) {
      appendText(
        movdw(at(Eax + (i * 4 + ArrayFirstElementOffset)), literal.text(i))
      )
    }

    appendText(
      :#("[BEGIN] Create String Object"),
      push(Eax),
      call(mallocTypeLabel(StringType.declaration)) :# "ecx = newly allocated object",
      pop(Eax),
      push(Eax),
      call(StringCharArrayConstructor.uniqueName) :# "Call String(char[])",
      pop(Eax),
      mov(Eax, Ecx),
      pop(Ecx),
      :#("[END] Create String Object")
    )

    appendText(
      :#(s"[END] Create String Literal: ${literal}"),
      #<,
      emptyLine
    )
  }
}

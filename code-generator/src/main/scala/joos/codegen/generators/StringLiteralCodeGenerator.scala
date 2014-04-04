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
      :#(s"[BEGIN] Create String Literal: ${literal}")
    )

    ArrayCreationExpression(CharType, literal.text.length).generate()
    for (i <- 0 until literal.text.length) {
      appendText(
        movdw(at(Eax + (i * 4 + ArrayFirstElementOffset)), literal.text(i))
      )
    }

    appendText(
      :#("[BEGIN] Create String Object"),
      push(Eax) :# "Give underlying char array as parameter to constructor",
      push(Ecx) :# "Save old this",
      call(mallocTypeLabel(StringType.declaration)) :# "ecx = newly allocated object",
//      pop(Ebx) :# "Retrieve old this",
//      push(Ebx) :# "Save old this",
      call(StringCharArrayConstructor.uniqueName) :# "Call String(char[])",
      mov(Eax, Ecx) :# "Set this string instance as return type",
      pop(Ecx) :# "Retrieve old this",
      pop(Ebx) :# "Pop parameter",
      :#("[END] Create String Object")
    )

    appendText(
      :#(s"[END] Create String Literal: ${literal}"),
      #<,
      emptyLine
    )
  }
}

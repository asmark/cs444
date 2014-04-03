package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.ClassInstanceCreationExpression
import joos.codegen.AssemblyCodeGeneratorEnvironment

class ClassInstanceCreationExpressionCodeGenerator(expression: ClassInstanceCreationExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {

    val tipe = expression.classType.declaration

    appendText(
      :#(s"[BEGIN] Class Instance Creation ${expression}"),
      emptyLine,
      call(mallocTypeLabel(tipe)) :# "Allocate raw bytes for object. Returns this in ecx",
      emptyLine
    )

    expression.arguments.foreach {
      argument =>
        appendText(
          :#("Evaluate parameter"),
          push(Ecx) :# "Save this",
          #>)

        argument.generate()

        appendText(
          #<,
          pop(Ecx) :# "Retrieve this",
          push(Eax) :# "Push parameter onto stack",
          emptyLine
        )
    }

    appendText(
      call(expression.constructor.uniqueName) :# "Call constructor. Returns this as ecx",
      add(Esp, 4*expression.arguments.size) :# "Pop arguments off stack",
      mov(Eax, Ecx) :# "Return this as eax"
    )

    appendText(
      :#(s"[END] Class Instance Creation ${expression}"),
      emptyLine
    )
  }

}
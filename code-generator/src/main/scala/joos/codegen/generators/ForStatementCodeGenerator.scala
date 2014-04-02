package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.statements.ForStatement
import joos.codegen.AssemblyCodeGeneratorEnvironment

class ForStatementCodeGenerator(statement: ForStatement)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    val forStart = nextLabel("for_start")
    val forEnd = nextLabel("for_end")

    appendText(
      #:("[BEGIN] For Statement"),
      emptyLine,
      #:("For Loop Initialization"),
      #>
    )
    statement.initialization.foreach(_.generate())

    appendText(
      #<,
      #:("For Loop Condition"),
      forStart ::,
      #>
    )

    statement.condition.foreach(_.generate())

    appendText(
      #<,
      mov(Ebx, 0) :# "Set ebx to false",
      cmp(Eax, Ebx) :# "Truth test for eax",
      je(forEnd) :# "Skip to end if condition is false",
      emptyLine)

    appendText(
      #:("For Loop Body"),
      #>
    )
    statement.body.generate()

    appendText(
      #<,
      emptyLine,
      #:("For Loop Update"),
      #>
    )
    statement.update.foreach(_.generate())

    appendText(
      #<,
      jmp(forStart) :# "Jump to for condition",
      forEnd ::,
      #:("[END] For Statement"),
      emptyLine
    )
  }

}

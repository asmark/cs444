package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.statements.WhileStatement
import joos.codegen.AssemblyCodeGeneratorEnvironment

class WhileStatementCodeGenerator(statement: WhileStatement)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    val whileStart = nextLabel("while_start")
    val whileEnd = nextLabel("while_end")

    appendText(
      #:("[BEGIN] While Statement"),
      emptyLine,
      #:("While Loop Condition"),
      whileStart ::,
      #>
    )
    statement.condition.generate()

    appendText(
      #<,
      mov(Ebx, 0) :# "Set ebx to false",
      cmp(Eax, Ebx) :# "Truth test for eax",
      je(whileEnd) :# "Skip to end if condition is false",
      emptyLine)

    appendText(
      #:("While Loop Body"),
      #>
    )
    statement.body.generate()

    appendText(
      #<,
      jmp(whileStart) :# "Jump to while condition",
      whileEnd ::,
      #:("[END] While Statement"),
      emptyLine
    )
  }

}

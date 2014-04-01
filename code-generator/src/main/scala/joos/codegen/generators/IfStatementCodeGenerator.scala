package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.statements.IfStatement
import joos.codegen.AssemblyCodeGeneratorEnvironment

class IfStatementCodeGenerator(statement: IfStatement)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {

    val falseStart = nextLabel("if_false_start")
    val statementEnd = nextLabel("if_statement_end")

    appendText(
      #:("[BEGIN] If Statement"),
      emptyLine,
      #:("Evaluate Condition"),
      #>
    )
    statement.condition.generate()

    appendText(
      #<,
      mov(Ebx, 0) #: "Set ebx to false",
      cmp(Eax, Ebx) #: "Truth test for eax",
      je(falseStart) #: "Skip to false if condition is false",
      emptyLine
    )

    appendText(
      #:("True branch body"),
      #>
    )
    statement.trueStatement.generate()
    appendText(
      #<,
      jmp(statementEnd) #: "Jump to statement end",
      emptyLine
    )

    appendText(
      #:("False branch body"),
      falseStart ::,
      #>
    )
    statement.falseStatement.foreach(_.generate())
    appendText(
      #<,
      statementEnd ::,
      #:("[END] If Statement"),
      emptyLine
    )
  }

}

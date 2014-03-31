package joos.codegen.generators

import joos.ast.statements.IfStatement
import joos.codegen.AssemblyFileManager
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment

class IfStatementCodeGenerator(statement: IfStatement)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    statement.condition.generate()
    statement.trueStatement.generate()
    statement.falseStatement.foreach(_.generate())
  }

}

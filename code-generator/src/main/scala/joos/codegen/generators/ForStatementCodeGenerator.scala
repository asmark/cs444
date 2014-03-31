package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.statements.ForStatement

class ForStatementCodeGenerator(statement: ForStatement)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    statement.initialization.foreach(_.generate())
    statement.condition.foreach(_.generate())
    statement.update.foreach(_.generate())
    statement.body.generate()
  }

}

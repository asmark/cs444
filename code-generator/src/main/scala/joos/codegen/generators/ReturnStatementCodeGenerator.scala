package joos.codegen.generators

import joos.assemgen._
import joos.ast.statements.ReturnStatement
import joos.codegen.AssemblyCodeGeneratorEnvironment

class ReturnStatementCodeGenerator(statement: ReturnStatement)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {
  
  override def generate() {
    statement.expression.foreach(_.generate())
    appendText(ret)
  }
}

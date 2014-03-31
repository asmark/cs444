package joos.codegen.generators

import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.statements.ExpressionStatement

class ExpressionStatementCodeGenerator(statement: ExpressionStatement)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    statement.expression.generate()
  }

}
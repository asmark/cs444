package joos.codegen.generators

import joos.ast.statements.{EmptyStatement, ReturnStatement}
import joos.codegen.AssemblyCodeGeneratorEnvironment

class EmptyStatementCodeGenerator(statement: EmptyStatement.type)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {
  
  override def generate() {}
}

package joos.codegen.generators

import joos.ast.statements.WhileStatement
import joos.codegen.AssemblyFileManager
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment

class WhileStatementCodeGenerator(statement: WhileStatement)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {}

}

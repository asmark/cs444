package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.statements.Block

class BlockCodeGenerator(statement: Block)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    statement.generate()
  }

}
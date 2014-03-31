package joos.codegen.generators

import joos.ast.declarations.SingleVariableDeclaration
import joos.codegen.AssemblyFileManager
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment

class SingleVarDeclarationCodeGenerator(variable: SingleVariableDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {}

}

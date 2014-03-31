package joos.codegen.generators

import joos.ast.declarations.PackageDeclaration
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment

class PackageDeclarationCodeGenerator(packaged: PackageDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
  }

}

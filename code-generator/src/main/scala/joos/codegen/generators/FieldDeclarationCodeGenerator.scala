package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.declarations.FieldDeclaration

class FieldDeclarationCodeGenerator(field: FieldDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    field.fragment.generate()
    val offset = field.typeDeclaration
  }

}

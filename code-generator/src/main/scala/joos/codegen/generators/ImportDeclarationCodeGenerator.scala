package joos.codegen.generators
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.declarations.ImportDeclaration
import joos.codegen.AssemblyFileManager

class ImportDeclarationCodeGenerator(importDeclaration: ImportDeclaration)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
  }

}

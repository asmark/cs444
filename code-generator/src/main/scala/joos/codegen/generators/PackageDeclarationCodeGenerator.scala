package joos.codegen.generators

import joos.ast.declarations.PackageDeclaration
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.{AssemblyCodeGenerator, AssemblyCodeGeneratorEnvironment}

class PackageDeclarationCodeGenerator(packaged: PackageDeclaration) extends Assembler {
  override def generateAssembly(): Unit = {}
}

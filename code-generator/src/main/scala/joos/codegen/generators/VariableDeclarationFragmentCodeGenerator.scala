package joos.codegen.generators

import joos.ast.declarations.VariableDeclarationFragment
import joos.codegen.AssemblyFileManager
import joos.codegen.assembler.Assembler
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.{AssemblyCodeGenerator, AssemblyCodeGeneratorEnvironment}

class VariableDeclarationFragmentCodeGenerator(expression: VariableDeclarationFragment)(implicit val assemblyManager: AssemblyFileManager) extends Assembler{
  override def generateAssembly(): Unit = {}
}

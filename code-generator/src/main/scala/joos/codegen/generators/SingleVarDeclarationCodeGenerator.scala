package joos.codegen.generators

import joos.ast.declarations.SingleVariableDeclaration
import joos.codegen.AssemblyFileManager
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.{AssemblyCodeGenerator, AssemblyCodeGeneratorEnvironment}

class SingleVarDeclarationCodeGenerator(variable: SingleVariableDeclaration)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

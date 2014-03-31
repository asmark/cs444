package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.{AssemblyCodeGenerator, AssemblyCodeGeneratorEnvironment}

class ForStatementCodeGenerator(statement: ForStatement)(implicit val assemblyManager: AssemblyFileManager) extends Assembler{
  override def generateAssembly(): Unit = {}
}

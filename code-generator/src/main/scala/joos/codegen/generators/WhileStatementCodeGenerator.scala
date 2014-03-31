package joos.codegen.generators

import joos.ast.statements.WhileStatement
import joos.codegen.AssemblyFileManager
import joos.codegen.assembler.Assembler
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.{AssemblyCodeGenerator, AssemblyCodeGeneratorEnvironment}

class WhileStatementCodeGenerator(statement: WhileStatement)(implicit val assemblyManager: AssemblyFileManager) extends Assembler{
  override def generateAssembly(): Unit = {}
}

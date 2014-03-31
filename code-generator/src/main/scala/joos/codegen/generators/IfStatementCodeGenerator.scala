package joos.codegen.generators

import joos.ast.statements.IfStatement
import joos.codegen.AssemblyFileManager
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.{AssemblyCodeGenerator, AssemblyCodeGeneratorEnvironment}
class IfStatementCodeGenerator(statement: IfStatement)(implicit val assemblyManager: AssemblyFileManager) extends Assembler{
  override def generateAssembly(): Unit = {}
}

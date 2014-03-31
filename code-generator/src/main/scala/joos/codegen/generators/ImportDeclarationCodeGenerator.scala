package joos.codegen.generators
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.{AssemblyCodeGenerator, AssemblyCodeGeneratorEnvironment}
import joos.ast.declarations.ImportDeclaration
import joos.codegen.AssemblyFileManager

class ImportDeclarationCodeGenerator(importDeclaration: ImportDeclaration)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}

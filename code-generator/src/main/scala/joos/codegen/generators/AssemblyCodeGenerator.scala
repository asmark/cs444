package joos.codegen.generators

import joos.assemgen.AssemblyLine
import joos.ast.{AbstractSyntaxTree, CompilationUnit, AstNode}
import joos.codegen.AssemblyCodeGeneratorEnvironment
import scala.language.implicitConversions

trait AssemblyCodeGenerator {
  def generate()

  def environment: AssemblyCodeGeneratorEnvironment

  def appendText(lines: AssemblyLine*) {
    environment.assemblyManager.appendData(lines: _*)
  }

  def appendGlobal(lines: AssemblyLine*) {
    environment.assemblyManager.appendGlobal(lines: _*)
  }

  protected implicit def toAssemblyCodeGenerator(element: AstNode)
      (implicit environment: AssemblyCodeGeneratorEnvironment): AssemblyCodeGenerator = {
    // TODO: implement
    null
  }
}

object AssemblyCodeGenerator {
  def apply(ast: AbstractSyntaxTree)(implicit environment: AssemblyCodeGeneratorEnvironment) {
    ast.root.typeDeclaration match {
      case None =>
      case Some(typeDeclaration) => new TypeDeclarationCodeGenerator(typeDeclaration).generate
    }
  }
}


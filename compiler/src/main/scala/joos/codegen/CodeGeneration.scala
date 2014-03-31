package joos.codegen

import joos.ast.AbstractSyntaxTree
import joos.codegen.generators.commonlib.AssemblyCommonLibraryEnvironment

object CodeGeneration {

  def apply(asts: Seq[AbstractSyntaxTree]) {

    val namespace = new AssemblyNamespace
    val CommonLibrary = AssemblyCommonLibraryEnvironment(namespace)

    val assemblyEnvironments = asts.foldRight(Seq(CommonLibrary)) {
      (ast, environments) =>
        environments :+ new AssemblyCodeGeneratorEnvironment(ast, namespace)
    }

    assemblyEnvironments foreach (_.write)

  }

}

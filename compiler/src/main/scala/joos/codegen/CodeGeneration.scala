package joos.codegen

import joos.ast.AbstractSyntaxTree
import joos.codegen.generators.commonlib.AssemblyCommonLibraryEnvironment
import joos.assemgen.StaticDataManager

object CodeGeneration {

  def apply(asts: Seq[AbstractSyntaxTree]) {

    val namespace = new AssemblyNamespace
    val staticDataManager = StaticDataManager(asts)

    val CommonLibrary = AssemblyCommonLibraryEnvironment(namespace, staticDataManager)

    val assemblyEnvironments = asts.foldRight(Seq(CommonLibrary)) {
      (ast, environments) =>
        environments :+ new AssemblyCodeGeneratorEnvironment(ast, namespace, staticDataManager)
    }

    assemblyEnvironments foreach (_.write)

  }

}

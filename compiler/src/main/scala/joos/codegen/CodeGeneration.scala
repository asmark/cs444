package joos.codegen

import joos.ast.AbstractSyntaxTree
import joos.codegen.generators.commonlib.AssemblyCommonLibraryEnvironment
import joos.assemgen.SITManager

object CodeGeneration {

  def apply(asts: Seq[AbstractSyntaxTree]) {

    val namespace = new AssemblyNamespace
    val sitManager = SITManager(asts)

    val CommonLibrary = AssemblyCommonLibraryEnvironment(namespace, sitManager)

    val assemblyEnvironments = asts.foldRight(Seq(CommonLibrary)) {
      (ast, environments) =>
        environments :+ new AssemblyCodeGeneratorEnvironment(ast, namespace, sitManager)
    }

    assemblyEnvironments foreach (_.write)

  }

}

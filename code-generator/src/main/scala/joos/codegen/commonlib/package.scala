package joos.codegen

import joos.assemgen._

package object commonlib {

  def AssemblyCommonLibraryEnvironment(namespace: AssemblyNamespace): AssemblyCodeGeneratorEnvironment = {

    val assemblyManager = new AssemblyFileManager("_lib.s")
    assemblyManager.appendText(addInts : _*)

    new AssemblyCodeGeneratorEnvironment(assemblyManager, namespace)
  }

  val addIntegers = "_addInt"
  private def addInts = Seq(
    comment("[BEGIN] Add Integer Library Function"),
    comment("[END] Add Integer Library Function"),
    emptyLine()
  )

}

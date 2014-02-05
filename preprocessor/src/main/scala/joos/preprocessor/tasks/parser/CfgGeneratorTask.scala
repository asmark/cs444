package joos.preprocessor.tasks.parser

import joos.preprocessor.tasks.PreProcessorTask
import joos.language.ContextFreeGrammar
import java.io.{FileInputStream, File, FileOutputStream}

object CfgGeneratorTask extends PreProcessorTask {

  final val HumanGrammar = getProperty("grammar")
  final val MachineGrammar = getProperty("machine-grammar")

  protected def dependsOn = List.empty[PreProcessorTask]

  protected def isTaskCached() = {
    isFileExist(getPathToManagedResource(MachineGrammar))
  }

  protected def executeTask() {
    val humanGrammarFile = new FileInputStream(getPathToResource(HumanGrammar))
    val machineGrammarFile = new FileOutputStream(new File(getPathToManagedResource(MachineGrammar)))

    ContextFreeGrammar.fromHumanReadableFormat(humanGrammarFile).toMachineReadableFormat(machineGrammarFile)
  }
}

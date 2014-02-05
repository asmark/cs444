package joos.preprocessor.tasks.parser

import java.io.{FileOutputStream, FileInputStream}
import joos.preprocessing.ActionTableGenerator
import joos.preprocessor.tasks.PreProcessorTask

object ActionTableGeneratorTask extends PreProcessorTask {

  final val MachineGrammar = getProperty("machine-grammar")
  final val ActionTable = getProperty("action-table")

  protected def dependsOn = List(CfgGeneratorTask)

  protected def isTaskCached() = {
    isFileExist(getPathToManagedResource(ActionTable))
  }

  protected def executeTask() {
    new ActionTableGenerator().createActionTable(
      new FileInputStream(getPathToManagedResource(MachineGrammar)),
      new FileOutputStream(getPathToManagedResource(ActionTable))
    )
  }
}

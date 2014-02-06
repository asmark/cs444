package joos.preprocessor.tasks.parser

import java.io.{FileOutputStream, FileInputStream}
import joos.preprocessing.ActionTableGenerator
import joos.preprocessor.tasks.PreProcessorTask
import joos.resources

object ActionTableGeneratorTask extends PreProcessorTask {
  protected def dependsOn = List(CfgGeneratorTask)

  protected def isTaskCached() = isFileExist(resources.lalr1Table)

  protected def executeTask() {
    new ActionTableGenerator().createActionTable(
      new FileInputStream(resources.serializedGrammar),
      new FileOutputStream(resources.lalr1Table)
    )
  }
}

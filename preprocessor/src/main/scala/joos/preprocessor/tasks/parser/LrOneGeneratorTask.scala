package joos.preprocessor.tasks.parser

import joos.preprocessor.tasks.PreProcessorTask
import com.google.common.io.Files
import java.io.{FileWriter, FileOutputStream, File}
import com.google.common.base.Charsets

object LrOneGeneratorTask  extends PreProcessorTask {

  final val MachineGrammar = getProperty("machine-grammar")
  final val ActionTable = getProperty("action-table")
  final val LrOneGrammar = getProperty("lr-one-grammar")

  protected def dependsOn = List(CfgGeneratorTask, ActionTableGeneratorTask)

  protected def isTaskCached() = {
    isFileExist(getPathToManagedResource(LrOneGrammar))
  }

  protected def executeTask() {
    val lrOneGrammarFile = new File(getPathToManagedResource(LrOneGrammar))
    Files.copy(new File(getPathToManagedResource(MachineGrammar)), lrOneGrammarFile)
    // Append to grammar file now
    val lrOneGrammarFileWriter = new FileWriter(lrOneGrammarFile, true)
    Files.copy(new File(getPathToManagedResource(ActionTable)), Charsets.UTF_8, lrOneGrammarFileWriter)
    lrOneGrammarFileWriter.close()
  }
}

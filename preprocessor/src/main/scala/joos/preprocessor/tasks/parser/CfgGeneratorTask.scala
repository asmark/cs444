package joos.preprocessor.tasks.parser

import java.io.FileOutputStream
import joos.language.ContextFreeGrammar
import joos.preprocessor.tasks.PreProcessorTask
import joos.resources

object CfgGeneratorTask extends PreProcessorTask {
  protected def dependsOn = List.empty[PreProcessorTask]

  protected def isTaskCached() = isFileExist(resources.serializedGrammar)

  protected def executeTask() {
    ContextFreeGrammar
      .fromReadableFormat(resources.grammar.openStream())
      .serialize(new FileOutputStream(resources.serializedGrammar))
  }
}

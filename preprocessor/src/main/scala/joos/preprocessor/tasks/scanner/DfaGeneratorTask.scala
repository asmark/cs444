package joos.preprocessor.tasks.scanner

import java.io.{File, FileOutputStream}
import joos.automata.Dfa
import joos.preprocessor.tasks.PreProcessorTask
import joos.tokens.TokenKind
import joos.tokens.TokenKind.TokenKindValue

object DfaGeneratorTask extends PreProcessorTask {

  private final val DfaFile = getProperty("dfa")
  private final lazy val JoosDfa = Dfa(TokenKind.values.map(_.asInstanceOf[TokenKindValue].getRegexp()).reduceRight((a, b) => a | b))

  def dependsOn: List[PreProcessorTask] = List.empty

  def isTaskCached(): Boolean = {
    isFileExist(getPathToManagedResource(DfaFile))
  }

  def executeTask() {
    val writer = new FileOutputStream(new File(getPathToManagedResource(DfaFile)))
    JoosDfa.serialize(writer)
    writer.close()
  }
}

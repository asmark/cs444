package joos.preprocessor.tasks

import java.io.FileOutputStream
import joos.automata.Dfa
import joos.tokens.TokenKind
import joos.tokens.TokenKind.TokenKindValue

object DfaGeneratorTask extends PreProcessorTask {

  private final val DfaFile = "Joos1W.dfa"
  private final lazy val JoosDfa = Dfa(TokenKind.values.map(_.asInstanceOf[TokenKindValue].getRegexp()).reduceRight((a, b) => a | b))

  def dependsOn: List[PreProcessorTask] = List.empty

  def isTaskCached(): Boolean = {
    isFileExist(DfaFile)
  }

  def executeTask() {
    val path = getResourceDirPath() + DfaFile
    val writer = new FileOutputStream(path)
    JoosDfa.serialize(writer)
    writer.close()
  }
}

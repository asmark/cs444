package joos.preprocessor.tasks

import java.io.FileWriter
import joos.automata.Dfa
import joos.tokens.TokenKind
import joos.tokens.TokenKind.TokenKindValue

object DfaGeneratorTask extends PreprocessorTask {

  private final val DfaFile = "Joos1W.dfa"
  private final lazy val JoosDfa = Dfa(TokenKind.values.map(_.asInstanceOf[TokenKindValue].getRegexp()).reduceRight((a, b) => a | b))

  def dependsOn: List[PreprocessorTask] = List.empty

  def isTaskCached(): Boolean = {
    getResourceDir().lines.contains(DfaFile)
  }

  def executeTask() {
    dependsOn.foreach(_.executeTask())

    val path = getResourceDir() + DfaFile
    val writer = new FileWriter(path)
    writer.write(JoosDfa.serialize())
    writer.flush()
    writer.close()
  }

}

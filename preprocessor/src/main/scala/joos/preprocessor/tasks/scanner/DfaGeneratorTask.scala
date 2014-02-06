package joos.preprocessor.tasks.scanner

import java.io.FileOutputStream
import joos.automata.Dfa
import joos.preprocessor.tasks.PreProcessorTask
import joos.tokens.TokenKind
import joos.tokens.TokenKind.TokenKindValue
import joos.resources

object DfaGeneratorTask extends PreProcessorTask {

  private final lazy val JoosDfa = Dfa(
    TokenKind
      .values
      .map(_.asInstanceOf[TokenKindValue].getRegexp())
      .reduceRight((a, b) => a | b)
  )

  def dependsOn: List[PreProcessorTask] = List.empty

  def isTaskCached() = isFileExist(resources.lexerDfa)

  def executeTask() {
    val writer = new FileOutputStream(resources.lexerDfa)
    JoosDfa.serialize(writer)
    writer.close()
  }
}

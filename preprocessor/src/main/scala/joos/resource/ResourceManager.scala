package joos.resource

import java.util.Properties
import joos.core._

object ResourceManager {
  private [this] val OriginalGrammar = "joos-1w-grammar.txt"
  private [this] val Grammar = "joos-1w.cfg"
  private [this] val LexerDfa = "joos-1w.dfa"
  private [this] val Lalr1Table = "joos-1w.table"

  private [this] val buildProperties = {
    using (getClass.getResourceAsStream("/build.properties")) {
      stream =>
        val properties = new Properties
        properties.load(stream)
        properties
    }
  }

  private [this] val managedResourceDirectory = buildProperties.get("managed-resource-directory")
  private [this] val gitHeadHash =

  def loadOriginalGrammarAsStream() = getClass.getResourceAsStream('/' + OriginalGrammar)
  def loadGrammarAsStream() = getClass.getResourceAsStream('/' + Grammar)

  def openGrammarAsOutputStream() = {

  }
}

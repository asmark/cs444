package joos

import joos.core._
import java.util.Properties
import java.io.File

package object resources {

  /** Update the version number if you change the grammar file or DFA */
  private [this] val GrammarVersion = 3
  private [this] val LexerVersion = 1

  private [this] val SerializedGrammarName = "joos-1w-grammar.cfg"
  private [this] val Lalr1ActionTableName = "joos-1w-grammar.lr1"
  private [this] val LexerDfaName = "joos-1w-grammar.dfa"
  private [this] val SerializedGrammarFullName = s"${GrammarVersion}-${SerializedGrammarName}"
  private [this] val Lalr1ActionTableFullName = s"${GrammarVersion}-${Lalr1ActionTableName}"
  private [this] val LexerDfaFullName = s"${LexerVersion}-${LexerDfaName}"

  private [this] val Versions = Map(
    SerializedGrammarName -> GrammarVersion,
    Lalr1ActionTableName -> GrammarVersion,
    LexerDfaName -> LexerVersion
  )

  private [this] val buildProperties = {
    using (getClass.getResourceAsStream("/build.properties")) {
      stream =>
        val properties = new Properties
        properties.load(stream)
        properties
    }
  }

  private [this] val generatedResourceDirectory = buildProperties.getProperty("generated-resource-directory")

  lazy val grammar = getClass.getResource("/joos-1w-grammar.txt")
  lazy val serializedGrammar = new File(generatedResourceDirectory, SerializedGrammarFullName)
  lazy val lalr1Table = new File(generatedResourceDirectory, Lalr1ActionTableFullName)
  lazy val lexerDfa = new File(generatedResourceDirectory, LexerDfaFullName)

  def clean() {
    for (file <- new File(generatedResourceDirectory).listFiles) {
      if (file.isFile) {
        val fullName = file.getName
        val seperatorIndex = fullName.indexOf('-')
        val version = fullName.substring(0, seperatorIndex).toInt
        val name = fullName.substring(seperatorIndex + 1)

        if (Versions(name) > version) {
          // Delete the file if the version is older
          file.delete()
        }
      }
    }
  }
}

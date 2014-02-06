package joos

import joos.core._
import java.util.Properties
import java.io.File

package object resources {

  private [this] val buildProperties = {
    using (getClass.getResourceAsStream("/build.properties")) {
      stream =>
        val properties = new Properties
        properties.load(stream)
        properties
    }
  }

  private [this] val managedResourceDirectory = buildProperties.getProperty("managed-resource-directory")

  lazy val grammar = getClass.getResource("/joos-1w-grammar.txt")
  lazy val serializedGrammar = new File(managedResourceDirectory, "joos-1w-grammar.cfg")
  lazy val lexerDfa = new File(managedResourceDirectory, "joos-1w-dfa.dfa")
  lazy val lalr1Table = new File(managedResourceDirectory, "joos-1w-grammar.lr1")
}

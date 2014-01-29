package joos.language

import java.io._
import java.util
import java.util.StringTokenizer
import joos.core._
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

case class ContextFreeGrammar(
  val start: String,
  val terminals: util.Set[String],
  val nonTerminals: util.Set[String],
  val rules: ListBuffer[ProductionRule]
) {

  def toMachineReadableFormat(outputStream: OutputStream): this.type = {
    using(new PrintWriter(new OutputStreamWriter(outputStream))) {
      writer =>
        writer.println(terminals.size)
        terminals.foreach(writer.println)
        writer.println(nonTerminals.size)
        nonTerminals.foreach(writer.println)
        writer.println(start)
        writer.println(rules.size)
        rules.foreach {
          rule =>
            writer.print(rule.left)
            rule.right.foreach {
              token =>
                writer.print(' ')
                writer.print(token)
            }
            writer.println()
        }
    }
    this
  }
}

object ContextFreeGrammar {
  def fromHumanReadableFormat(inputStream: InputStream): ContextFreeGrammar = {

    using(new BufferedReader(new InputStreamReader(inputStream))) {
      reader =>
        val terminals = new util.LinkedHashSet[String]
        val nonTerminals = new util.LinkedHashSet[String]
        var rules = new ListBuffer[ProductionRule]
        var left = ""
        var line: String = reader.readLine()
        var start = ""

        while (line != null) {
          // Skip comments and empty lines
          if (!line.isEmpty && line(0) != '#') {
            // If the line does not start with whitespace, then it's the left side
            if (!line(0).isWhitespace) {
              val colonIndex = line.lastIndexOf(':')
              // Remove ':' at the end if there is one
              left =
                if (colonIndex >= 0) line.substring(0, colonIndex)
                else line
              terminals -= left
              nonTerminals += left

              start = if (start.isEmpty) left else start
            } else {
              val tokenizer = new StringTokenizer(line)
              val list = new ListBuffer[String]
              while (tokenizer.hasMoreTokens) {
                val token = tokenizer.nextToken()
                // If the token is not a non-terminal, add it to terminals
                if (!nonTerminals.contains(token)) {
                  terminals += token
                }
                list += token
              }

              if (list.size > 0) {
                rules += ProductionRule(left, list)
              }
            }
          }

          line = reader.readLine()
        }

        new ContextFreeGrammar(start, terminals, nonTerminals, rules)
    }
  }
}

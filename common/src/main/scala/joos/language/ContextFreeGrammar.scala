package joos.language

import java.io._
import java.util.StringTokenizer
import joos.core._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.BufferedSource

case class ContextFreeGrammar(
  start: String,
  terminals: collection.Set[String],
  nonTerminals: collection.Set[String],
  rules: IndexedSeq[ProductionRule]
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
            writer.print(rule.base)
            rule.derivation.foreach {
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
  private [this] val OptionalSuffix = "opt"

  /**
   * Expands the derivation that might or might not have optional symbols
   */
  private [this] def expand(derivation: mutable.Buffer[String]): Traversable[IndexedSeq[String]] = {
    val queue = new mutable.Queue[ArrayBuffer[String]]
    queue.enqueue(ArrayBuffer.empty[String])

    for (symbol <- derivation) {
      for (i <- 0 until queue.size) {
        val derivationBuilder = queue.dequeue()
        if (symbol.endsWith(OptionalSuffix)) {
          queue.enqueue(derivationBuilder)
          queue.enqueue(ArrayBuffer(derivationBuilder: _*) += symbol.substring(0, symbol.length - OptionalSuffix.length))
        } else {
          queue.enqueue(ArrayBuffer(derivationBuilder: _*) += symbol)
        }
      }
    }

    return queue
  }

  /**
   * Filter out comments and empty lines
   */
  private [this] def toLines(inputStream: InputStream): Iterator[String] = {
    using (new BufferedSource(inputStream)) {
      source => source.getLines().filter(line => !line.isEmpty && line(0) != '#')
    }
  }

  def fromHumanReadableFormat(inputStream: InputStream): ContextFreeGrammar = {
    val terminals = mutable.LinkedHashSet.empty[String]
    val nonTerminals = mutable.LinkedHashSet.empty[String]
    val rulesBuilder = ArrayBuffer.empty[ProductionRule]
    var left = ""
    var start = ""

    for (line <- toLines(inputStream)) {
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
        val derivation = new ArrayBuffer[String]
        while (tokenizer.hasMoreTokens) {
          val symbol = tokenizer.nextToken()
          val symbolWithoutSuffix =
            if (symbol.endsWith(OptionalSuffix)) symbol.substring(0, symbol.length - OptionalSuffix.length)
            else symbol
          // If the token is not a non-terminal, add it to terminals
          if (!nonTerminals.contains(symbolWithoutSuffix)) {
            terminals += symbolWithoutSuffix
          }
          derivation += symbol
        }

        if (derivation.size > 0) {
          rulesBuilder ++= expand(derivation)
            .map(derivation => ProductionRule(left, derivation))
        }
      }
    }

    return new ContextFreeGrammar(start, terminals, nonTerminals, rulesBuilder)
  }
}

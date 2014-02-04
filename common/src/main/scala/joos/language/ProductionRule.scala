package joos.language

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class ProductionRule(left: String, right: IndexedSeq[String]) {

  private [this] val OptionalSuffix = "opt"

  def expand(ruleBuilder: mutable.Buffer[ProductionRule]) {
    val queue = new mutable.Queue[ArrayBuffer[String]]
    queue.enqueue(ArrayBuffer.empty[String])

    for (symbol <- right) {
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

    ruleBuilder ++= queue.map(derivationBuilder => ProductionRule(left, derivationBuilder))
  }
}

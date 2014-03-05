package joos.language

case class ProductionRule(base: String, derivation: IndexedSeq[String]) {
  override def toString = base + " -> " + derivation.mkString(" ")
}

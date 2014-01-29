package joos.language

import scala.collection.mutable.ListBuffer

case class ProductionRule(val left: String, val right: ListBuffer[String])

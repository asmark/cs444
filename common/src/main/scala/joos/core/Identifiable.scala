package joos.core

trait Identifiable {
  val Id = UniqueIdGenerator.genId()
}

package joos.core

trait Identifiable {
  val id = DefaultUniqueIdGenerator.nextId
}

package joos.core

trait UniqueIdGenerator {
  private[this] var counter = -1

  def nextId(): Int = {
    counter += 1
    counter
  }
}

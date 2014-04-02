package joos.core

trait UniqueIdGenerator {
  private[this] var counter = 0

  def nextId(): Int = {
    counter += 1
    counter
  }
}

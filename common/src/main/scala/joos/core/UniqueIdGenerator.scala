package joos.core

trait UniqueIdGenerator {
  protected[this] var counter: Int = 0

  def nextId(): Int = {
    counter += 1
    counter
  }
}

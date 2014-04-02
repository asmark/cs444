package joos.core

class UniqueIdGenerator {
  private[this] var counter = 0

  def nextId(): Int = {
    counter += 1
    counter
  }
}

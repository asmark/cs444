package joos.core

object UniqueIdGenerator {
  private var counter = -1

  def genId() = {
    counter += 1
    counter
  }
}

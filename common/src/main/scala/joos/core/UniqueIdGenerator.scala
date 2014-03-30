package joos.core

object UniqueIdGenerator {
  private var counter = 1000

  def genId() = {
    counter += 1
    counter
  }
}

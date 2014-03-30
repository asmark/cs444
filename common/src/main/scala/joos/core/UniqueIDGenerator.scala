package joos.core

object UniqueIDGenerator {
  private var counter = -1

  def genID() = {
    counter += 1
    counter
  }
}

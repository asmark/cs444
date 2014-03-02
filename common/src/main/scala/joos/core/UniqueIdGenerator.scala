package joos.core

trait UniqueIdGenerator {
  private[this] var id = 0

  def nextId(): Int = {
    val id = this.id
    this.id += 1
    id
  }
}

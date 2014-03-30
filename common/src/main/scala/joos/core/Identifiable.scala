package joos.core

trait Identifiable {
  private val Id = UniqueIdGenerator.genId()
  def getId = Id
}

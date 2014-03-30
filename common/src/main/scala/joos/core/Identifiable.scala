package joos.core

trait Identifiable {
  private val id = UniqueIDGenerator.genID()
  def getID = id
}

package joos.codegen

import joos.core.DefaultUniqueIdGenerator

package object assembler {
  def getRandomLabel() = "label" + DefaultUniqueIdGenerator.nextId
}

package joos.codegen

import joos.core.DefaultUniqueIdGenerator

package object assembler {
  def nextLabel(labelPrefix: String = "label") = labelPrefix + "_" + DefaultUniqueIdGenerator.nextId
}

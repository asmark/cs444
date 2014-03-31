package joos.codegen

import joos.core.DefaultUniqueIdGenerator

package object  generators {
  def nextLabel(labelPrefix: String = "label") = labelPrefix + "_" + DefaultUniqueIdGenerator.nextId
}

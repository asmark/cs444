package joos.codegen

import joos.core.DefaultUniqueIdGenerator

package object  generators {
  val offsetPostFix = "_offset"

  def nextLabel(labelPrefix: String = "label") = labelPrefix + "_" + DefaultUniqueIdGenerator.nextId
}

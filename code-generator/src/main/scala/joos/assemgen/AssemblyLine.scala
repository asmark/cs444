package joos.assemgen

import joos.core.Writable

/**
 * Represents a line in assembly code
 */
trait AssemblyLine extends Writable {
  /**
   * Writes a comment
   */
  def :#(comment: String): AssemblyLine = {
    new AssemblyComment(comment)
  }
}

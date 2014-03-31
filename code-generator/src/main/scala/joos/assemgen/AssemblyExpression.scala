package joos.assemgen

import joos.core.Writable
import java.io.PrintWriter

trait AssemblyExpression extends Writable {
  def +(other: AssemblyExpression) = {
    val that = this
    new AssemblyExpression {
      override def write(writer: PrintWriter) {
        that.write(writer)
        writer.write(" + ")
        other.write(writer)
      }
    }
  }
}

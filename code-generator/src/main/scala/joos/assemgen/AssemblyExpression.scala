package joos.assemgen

import java.io.PrintWriter
import joos.core.Writable

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

  def -(other: AssemblyExpression) = {
    val that = this
    new AssemblyExpression {
      override def write(writer: PrintWriter) {
        that.write(writer)
        writer.write(" - ")
        other.write(writer)
      }
    }
  }
}

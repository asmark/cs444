package joos.assemgen

import java.io.PrintWriter

/**
 * Represents an assembly that serves as a marker
 */
trait PseudoAssemblyLine extends AssemblyLine {
  override def write(writer: PrintWriter) {}
}

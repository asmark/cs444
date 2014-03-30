package joos.assemgen

import java.io.PrintWriter

/**
 * Represents a reference to some content in memory at {{address}}
 */
class MemoryReference(address: AssemblyExpression) extends AssemblyExpression {
  override def write(writer: PrintWriter) {
    writer.print('[')
    address.write(writer)
    writer.print(']')
  }
}

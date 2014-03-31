package joos.codegen

import scala.collection.mutable

class AssemblyNamespace {

  val externs = mutable.HashSet(
    "__malloc",
    "__exception",
    "NATIVEjava.io.OutputStream.nativeWrite")

}

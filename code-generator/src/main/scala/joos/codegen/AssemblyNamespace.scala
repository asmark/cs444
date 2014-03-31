package joos.codegen

import scala.collection.mutable
import joos.assemgen._

class AssemblyNamespace {

  val externs = mutable.HashSet(extern(new LabelReference("__malloc")),
    extern(new LabelReference("__exception")),
    extern(new LabelReference("NATIVEjava.io.OutputStream.nativeWrite")))

}

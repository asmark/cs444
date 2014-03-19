package joos.ast

import joos.core.Enumeration

class NameClassification(val name: String) extends NameClassification.Value

object NameClassification extends Enumeration {
  type T = NameClassification

  final val Ambiguous = this + new NameClassification("ambiguous")
  final val PackageName = this + new NameClassification("package")
  final val TypeName = this + new NameClassification("type")
  final val InstanceFieldName = this + new NameClassification("instance field")
  final val InstanceMethodName = this + new NameClassification("instance method")
  final val StaticFieldName = this + new NameClassification("static field")
  final val StaticMethodName = this + new NameClassification("static method")
  final val LocalVariableName = this + new NameClassification("local variable")
}

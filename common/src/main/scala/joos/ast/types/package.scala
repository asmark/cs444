package joos.ast

package object types {
  final val NullType = new Type {
    override def standardName = "null"
  }

  final val VoidType = new Type {
    override def standardName = "void"
  }

  final val StringType = Type("java.lang.String")
  final val ObjectType = Type("java.lang.Object")
}

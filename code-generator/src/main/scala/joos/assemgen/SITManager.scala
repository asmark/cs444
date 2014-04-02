package joos.assemgen

import joos.ast.AbstractSyntaxTree
import joos.core.UniqueIdGenerator
import scala.collection.mutable
import joos.ast.declarations.{MethodDeclaration, TypeDeclaration}

class SITManager(asts: Seq[AbstractSyntaxTree]) {
  val typeIds = mutable.HashMap.empty[TypeDeclaration, Int]
  val methodIds = mutable.HashMap.empty[MethodDeclaration, Int]

  val typeIdGenerator = new UniqueIdGenerator()
  val methodIdGenerator = new UniqueIdGenerator()

  asts.foreach(
    ast => {
      ast.root.typeDeclaration.foreach {
        tipe => {
          for (method <- tipe.methods) {
            methodIds.put(method, typeIdGenerator.nextId())
          }

          typeIds.put(tipe, typeIdGenerator.nextId())
        }
      }
    }
  )
}

object SITManager {
  def apply(asts: Seq[AbstractSyntaxTree]) = new SITManager(asts)
}

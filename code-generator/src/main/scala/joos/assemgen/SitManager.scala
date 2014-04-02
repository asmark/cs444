package joos.assemgen

import joos.ast.AbstractSyntaxTree
import joos.core.UniqueIdGenerator
import scala.collection.mutable
import joos.ast.declarations.{MethodDeclaration, TypeDeclaration}

class SitManager(asts: Seq[AbstractSyntaxTree]) {
  val methodIds = mutable.HashMap.empty[MethodDeclaration, Int]

  val methodIdGenerator = new UniqueIdGenerator {
    counter = -1
  }

  asts.foreach(
    ast => {
      ast.root.typeDeclaration.foreach {
        tipe => {
          for (method <- tipe.methods) {
            methodIds.put(method, methodIdGenerator.nextId())
          }
        }
      }
    }
  )

  def getMethodIndex(methodDeclaration: MethodDeclaration): Int = {
    methodIds.get(methodDeclaration).get
  }

  lazy val orderedMethods: IndexedSeq[MethodDeclaration] = {
    methodIds.toSeq.sortWith((left, right) => left._2 < right._2).map(pair => pair._1).toIndexedSeq
  }
}

object SitManager {
  def apply(asts: Seq[AbstractSyntaxTree]) = new SitManager(asts)
}

package joos.assemgen

import joos.ast.AbstractSyntaxTree
import joos.core.UniqueIdGenerator
import scala.collection.mutable
import joos.ast.declarations.{MethodDeclaration, TypeDeclaration}

class StaticDataManager(asts: Seq[AbstractSyntaxTree]) {
  val typeIds = mutable.HashMap.empty[TypeDeclaration, Int]
  val methodIds = mutable.HashMap.empty[MethodDeclaration, Int]

  val methodIdGenerator = new UniqueIdGenerator {
    counter = -1
  }

  val typeIdGenerator = new UniqueIdGenerator {
    counter = -1
  }

  asts.foreach(
    ast => {
      ast.root.typeDeclaration.foreach {
        tipe => {
          for (method <- tipe.methods) {
            methodIds.put(method, methodIdGenerator.nextId())
          }

          typeIds.put(tipe, typeIdGenerator.nextId())
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

  lazy val orderedTypes: IndexedSeq[TypeDeclaration] = {
    typeIds.toSeq.sortWith((left, right) => left._2 < right._2).map(pair => pair._1).toIndexedSeq
  }
}

object StaticDataManager {
  def apply(asts: Seq[AbstractSyntaxTree]) = new StaticDataManager(asts)
}

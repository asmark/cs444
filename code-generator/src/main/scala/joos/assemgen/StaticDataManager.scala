package joos.assemgen

import joos.ast.AbstractSyntaxTree
import joos.core.UniqueIdGenerator
import scala.collection.mutable
import joos.ast.declarations.{MethodDeclaration, TypeDeclaration}

class StaticDataManager(asts: Seq[AbstractSyntaxTree]) {
  val typeIds = mutable.HashMap.empty[String, (TypeDeclaration, Int)]
  val methodIds = mutable.HashMap.empty[String, (MethodDeclaration, Int)]

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
            methodIds.put(method.uniqueName, (method, methodIdGenerator.nextId()))
          }

          typeIds.put(tipe.uniqueName, (tipe, typeIdGenerator.nextId()))
        }
      }
    }
  )

  def getMethodIndex(methodId: String): Int = {
    methodIds.get(methodId).get._2
  }

  lazy val orderedMethods: IndexedSeq[MethodDeclaration] = {
    methodIds.toSeq.sortWith((left, right) => left._2._2 < right._2._2).map(pair => pair._2._1).toIndexedSeq
  }

  lazy val orderedTypes: IndexedSeq[TypeDeclaration] = {
    typeIds.toSeq.sortWith((left, right) => left._2._2 < right._2._2).map(pair => pair._2._1).toIndexedSeq
  }
}

object StaticDataManager {
  def apply(asts: Seq[AbstractSyntaxTree]) = new StaticDataManager(asts)
}

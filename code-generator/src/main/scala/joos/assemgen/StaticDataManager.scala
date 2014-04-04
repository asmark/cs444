package joos.assemgen

import joos.ast.AbstractSyntaxTree
import joos.ast.declarations.{MethodDeclaration, TypeDeclaration}
import joos.core.UniqueIdGenerator
import scala.collection.mutable
import joos.ast.expressions.SimpleNameExpression
import joos.ast.compositions.DeclarationLike

class StaticDataManager(asts: Seq[AbstractSyntaxTree]) {
  val typeIds = mutable.HashMap.empty[String, (TypeDeclaration, Int)]
  val methodIds = mutable.HashMap.empty[String, (MethodDeclaration, Int)]

  val methodIdGenerator = new UniqueIdGenerator {
    counter = -1
  }

  val typeIdGenerator = new UniqueIdGenerator {
    counter = -1
  }

  asts.foreach {
    ast =>
      ast.root.typeDeclaration.foreach {
        tipe => {
          for (method <- tipe.methodMap.values) {
            methodIds.put(method.uniqueName, (method -> methodIdGenerator.nextId()))
          }
          typeIds.put(tipe.uniqueName, (tipe -> typeIdGenerator.nextId()))
        }
      }
  }

  def getMethodIndex(method: MethodDeclaration): Int = {
    methodIds.get(method.uniqueName).get._2
  }

  def getTypeIndex(tipe: TypeDeclaration): Int = {
      typeIds.get(tipe.uniqueName).get._2
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

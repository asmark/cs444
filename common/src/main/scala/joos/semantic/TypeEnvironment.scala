package joos.semantic

import joos.ast.declarations.{TypeDeclaration, FieldDeclaration, MethodDeclaration}
import joos.ast.expressions.SimpleNameExpression
import scala.collection.mutable

trait TypeEnvironment extends Environment {
  self: TypeDeclaration =>
  val constructorMap = mutable.HashMap.empty[String, MethodDeclaration]
  val methodMap = mutable.HashMap.empty[String, MethodDeclaration]
  val fieldMap = mutable.HashMap.empty[SimpleNameExpression, FieldDeclaration]


  lazy val inheritedFields: Map[SimpleNameExpression, FieldDeclaration] = {
    getSuperType(this) match {
      case Some(superType) => superType.containedFields
      case None => Map.empty
    }
  }

  lazy val containedFields: Map[SimpleNameExpression, FieldDeclaration] = {
    // By adding top-level classes first, we replace older overridden methods with new ones
    inheritedFields ++ fieldMap
  }

  /**
   * Adds the specified {{method}} to the type environment
   */
  def add(method: MethodDeclaration): this.type = {
    if (method.isConstructor) {
      assert(constructorMap.put(method.typedSignature, method).isEmpty)
    } else {
      assert(methodMap.put(method.typedSignature, method).isEmpty)
    }
    this
  }

  /**
   * Adds the specified {{field}} to the type environment
   */
  def add(field: FieldDeclaration): this.type = {
    assert(fieldMap.put(field.fragment.identifier, field).isEmpty)
    this
  }

  private lazy val supers = {
    var ret: Seq[TypeDeclaration] = Seq()

    getSuperType(this) match {
      case Some(superType) => ret = ret :+ superType
      case None => {}
    }

    this.superInterfaces.foreach(
      superInterface => {
        ret = ret :+ getTypeDeclaration(superInterface)
      }
    )

    ret
  }

  lazy val allAncestors: Set[TypeDeclaration] = {
    val allAncestors = this.supers.map(
      superType => superType.allAncestors
    )
    val ret = allAncestors.foldLeft(Set[TypeDeclaration]())(
      (left, right) => left ++ right
    )
    ret
  }

  private def isAllAbstract(method: MethodDeclaration): Boolean = {
    var ret = true
    this.supers.foreach(
      superType => {
        val superTypeContained = superType.containedMethods
        superTypeContained.values.flatten.foreach {
          contained =>
            if ((contained.returnTypeLocalSignature equals method.returnTypeLocalSignature) && !contained.isAbstractMethod &&
                areEqual(contained.returnType, method.returnType))
              ret = false
        }
      }
    )
    ret
  }

  private def addBinding(method: MethodDeclaration, map: Map[SimpleNameExpression, Set[MethodDeclaration]]) = {
    if (map.get(method.name).isEmpty) {
      map + (method.name -> Set(method))
    } else {
      val currentMethods = map(method.name) + method
      map + (method.name -> currentMethods)
    }
  }

  lazy val containedMethods: Map[SimpleNameExpression, Set[MethodDeclaration]]= {
    (methodMap.values ++ inheritedMethods.values.flatten).foldRight(Map.empty[SimpleNameExpression, Set[MethodDeclaration]]) {
      (method: MethodDeclaration, map: Map[SimpleNameExpression, Set[MethodDeclaration]]) =>
        addBinding(method, map)
     }
  }

  lazy val inheritedMethods: Map[SimpleNameExpression, Set[MethodDeclaration]] = {
    var ret = Map.empty[SimpleNameExpression, Set[MethodDeclaration]]

    this.supers.foreach {
      superType =>
        val localSignatures = this.methods.map(method => method.returnTypeLocalSignature)
        superType.containedMethods.values.flatten foreach {
          contained =>
            if (!localSignatures.contains(contained.returnTypeLocalSignature)) {
              if (!contained.isAbstractMethod) {
                ret = addBinding(contained, ret)
              } else {
                // All abs
                if (isAllAbstract(contained)) {
                  ret = addBinding(contained, ret)
                }
              }
            }
        }
    }
    ret
  }
}

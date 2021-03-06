package joos.semantic

import joos.ast.Modifier
import joos.ast.declarations.{TypeDeclaration, FieldDeclaration, MethodDeclaration}
import joos.ast.expressions.SimpleNameExpression
import scala.collection.mutable

trait TypeEnvironment extends Environment {
  self: TypeDeclaration =>
  val constructorMap = mutable.HashMap.empty[String, MethodDeclaration]
  val methodMap = mutable.HashMap.empty[String, MethodDeclaration]
  val fieldMap = mutable.LinkedHashMap.empty[SimpleNameExpression, FieldDeclaration]

  lazy val inheritedFields: mutable.LinkedHashMap[SimpleNameExpression, FieldDeclaration] = {
    getSuperType(this) match {
      case Some(directParent) => directParent.containedFields
      case None => mutable.LinkedHashMap.empty
    }
  }

  lazy val containedFields: mutable.LinkedHashMap[SimpleNameExpression, FieldDeclaration] = {
    // By adding top-level classes first, we replace older overridden methods with new ones
    val contained = mutable.LinkedHashMap[SimpleNameExpression, FieldDeclaration]()
    inheritedFields.foreach(x => contained.put(x._1, x._2))
    fieldMap.foreach(x => contained.put(x._1, x._2))
    contained
  }

  lazy val staticFields = containedFields.values.filter(_.isStatic)
  lazy val instanceFields = containedFields.values.filter(!_.isStatic)

  lazy val objectSize = instanceFields.size * 4

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

    getSuperType(this) foreach {
      superType =>
        ret = ret :+ superType
    }

    this.superInterfaces.foreach(
      superInterface => {
        ret = ret :+ getTypeDeclaration(superInterface)
      }
    )
    ret
  }

  lazy val allAncestors: Set[TypeDeclaration] = {
    val allAncestors = supers.map {
      superType => superType.allAncestors + superType
    }
    allAncestors.foldLeft(Set.empty[TypeDeclaration]) {
      (left, right) => left ++ right
    }
  }

  private def isAllAbstract(method: MethodDeclaration): Boolean = {
    supers.foreach {
      superType =>
        val superTypeContained = superType.containedMethods
        superTypeContained.values.flatten.foreach {
          contained =>
            if ((contained.returnTypeLocalSignature equals method.returnTypeLocalSignature) && !contained.isAbstract &&
                areEqual(contained.returnType, method.returnType))
              return false
        }
    }
    return true
  }

  lazy val inheritedMethods: Map[SimpleNameExpression, Set[MethodDeclaration]] = {

    def addBinding(method: MethodDeclaration, map: Map[SimpleNameExpression, Set[MethodDeclaration]]) = {
      if (map.get(method.name).isEmpty) {
        map + (method.name -> Set(method))
      } else {
        val currentMethods = map(method.name) + method
        map + (method.name -> currentMethods)
      }
    }

    var ret = Map.empty[SimpleNameExpression, Set[MethodDeclaration]]

    this.supers.foreach {
      superType =>
        val localSignatures = this.methods.map(method => method.returnTypeLocalSignature)
        superType.containedMethods.values.flatten foreach {
          contained =>
            if (!localSignatures.contains(contained.returnTypeLocalSignature)) {
              if (!contained.isAbstract) {
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

  lazy val containedMethods: Map[SimpleNameExpression, Set[MethodDeclaration]] = {

    // HACK: This is a total hack. Don't expect to understand anything by looking at this.
    def addBindingAndWidenVisibility(newMethod: MethodDeclaration, map: Map[SimpleNameExpression, Set[MethodDeclaration]]) = {
      if (map.get(newMethod.name).isEmpty) {
        map + (newMethod.name -> Set(newMethod))
      } else {
        // Check if we need to widen visibility
        val currentMethods = map(newMethod.name).find(_.localSignature == newMethod.localSignature) match {
          // No existing method with these parameters exists. This is a simple overload
          case None => map(newMethod.name) + newMethod
          // An old method already exists. Check if we must widen visibility
          case Some(oldMethod) => {
            // Set overloaded method
            if (oldMethod.modifiers.contains(Modifier.Protected) && newMethod.modifiers.contains(Modifier.Public)) {
              // Remove the old (Protected) method in favour of the new (Public) one
              // Key thing is to remove old method from the map
              map(newMethod.name) - oldMethod + newMethod
            } else if (oldMethod.modifiers.contains(Modifier.Abstract) && !newMethod.modifiers.contains(Modifier.Abstract)) {
              // Get rid of old abstract methods with concrete ones
              map(newMethod.name) - oldMethod + newMethod
            } else {
              // Overwrite the method anyways.. because why not?
              // Does this even work? Who knows. All the tests pass.
              map(newMethod.name) + newMethod
            }
          }
        }
        map + (newMethod.name -> currentMethods)
      }
    }

    (methodMap.values).foldLeft(inheritedMethods) {
      (map: Map[SimpleNameExpression, Set[MethodDeclaration]], method: MethodDeclaration) =>
        addBindingAndWidenVisibility(method, map)
    }
  }



  lazy val dispatchableMethods: Map[SimpleNameExpression, Seq[MethodDeclaration]] = {

    def addBindingAndWidenVisibility(newMethod: MethodDeclaration, map: Map[SimpleNameExpression, Seq[MethodDeclaration]]) = {
      if (map.get(newMethod.name).isEmpty) {
        map + (newMethod.name -> Seq(newMethod))
      } else {
        // Check if we need to widen visibility
        val currentMethods = map(newMethod.name).find(_.localSignature equals newMethod.localSignature) match {
          // No existing method with these parameters exists. This is a simple overload
          case None => map(newMethod.name) :+ newMethod
          // An old method already exists. Check if we must widen visibility
          case Some(oldMethod) => {
            map(newMethod.name) :+ newMethod
          }
        }
        map + (newMethod.name -> currentMethods)
      }
    }

    (methodMap.values).foldLeft(superTypeMethods) {
      (map: Map[SimpleNameExpression, Seq[MethodDeclaration]], method: MethodDeclaration) =>
        addBindingAndWidenVisibility(method, map)
    }
  }

  lazy val superTypeMethods = {

    def addBinding(method: MethodDeclaration, map: Map[SimpleNameExpression, Seq[MethodDeclaration]]) = {
      if (map.get(method.name).isEmpty) {
        map + (method.name -> Seq(method))
      } else {
        val currentMethods = map(method.name) :+ method
        map + (method.name -> currentMethods)
      }
    }

    var ret = Map.empty[SimpleNameExpression, Seq[MethodDeclaration]]

    this.supers.foreach {
      superType =>
        superType.dispatchableMethods.values.flatten foreach {
          contained =>
                ret = addBinding(contained, ret)
        }
    }
    ret
  }


  private lazy val fieldSlots = {
    var fieldIndex = 0
    val fieldSlots = mutable.Map.empty[SimpleNameExpression, Int]
    containedFields.foreach {
      field =>
        fieldSlots.put(field._1, fieldIndex)
        fieldIndex += 1
    }
    fieldSlots
  }

  def getFieldSlot(field: SimpleNameExpression): Int = {
    fieldSlots(field)
  }
}

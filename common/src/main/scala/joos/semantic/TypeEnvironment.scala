package joos.semantic

import joos.ast.declarations.{TypeDeclaration, FieldDeclaration, MethodDeclaration}
import joos.ast.expressions.SimpleNameExpression
import scala.collection.mutable

trait TypeEnvironment extends Environment {
  self: TypeDeclaration =>
  val constructorMap = mutable.HashMap.empty[String, MethodDeclaration]
  val methodMap = mutable.HashMap.empty[String, MethodDeclaration]
  val fieldMap = mutable.HashMap.empty[SimpleNameExpression, FieldDeclaration]

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

  lazy val supers = {
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

  private def isAllAbstract(method: MethodDeclaration): Boolean = {
    var ret = true
    this.supers.foreach(
      superType => {
        val superTypeContained = superType.containedMethodSet.toArray
        superTypeContained.foreach(
          contained =>
            if ((contained.returnTypeLocalSignature equals method.returnTypeLocalSignature) && !contained.isAbstractMethod &&
                areEqual(contained.returnType, method.returnType))
              ret = false
        )
      }
    )
    ret
  }

  lazy val containedMethodSet: mutable.HashSet[MethodDeclaration] = {
    var ret = mutable.HashSet.empty[MethodDeclaration]

    methodMap.values.foreach(local => ret += local)
    this.inheritMethods.foreach(inherited => ret += inherited)

    ret
  }

  lazy val inheritMethods: mutable.HashSet[MethodDeclaration] = {
    var ret = mutable.HashSet.empty[MethodDeclaration]

    this.supers.foreach(
      superType => {
        val localSignatures = this.methods.map(method => method.returnTypeLocalSignature)
        val array = superType.containedMethodSet.toArray
        array foreach {
          contained =>
            if (!localSignatures.contains(contained.returnTypeLocalSignature)) {
              if (!contained.isAbstractMethod) {
                ret += contained
              } else {
                // All abs
                if (isAllAbstract(contained)) {
                  ret += contained
                }
              }
            }
        }
      }
    )

    ret
  }
}

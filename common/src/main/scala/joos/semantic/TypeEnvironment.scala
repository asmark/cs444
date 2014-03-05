package joos.semantic

import joos.ast.declarations.{TypeDeclaration, FieldDeclaration, MethodDeclaration}
import joos.ast.expressions.SimpleNameExpression
import scala.collection.mutable

trait TypeEnvironment extends Environment {
  self: TypeDeclaration =>
  val constructorMap = mutable.HashMap.empty[String, MethodDeclaration]
  val methodMap = mutable.HashMap.empty[String, MethodDeclaration]
  val fieldMap = mutable.HashMap.empty[SimpleNameExpression, FieldDeclaration]
  private var inheritedMethodMap = mutable.HashMap.empty[String, Seq[MethodDeclaration]]
  private var inheritedFieldMap = mutable.HashMap.empty[String, Seq[FieldDeclaration]]

  /**
   * Adds the specified {{method}} to the type environment
   */
  def add(method: MethodDeclaration): this.type = {
    if (method.isConstructor) {
      constructorMap.put(method.typedSignature, method)
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
    this.supers.foreach(superType => {
        val superTypeContained = superType.containedMethodMap.values.toSeq.flatten.toArray
        superTypeContained.foreach(contained =>
          if ((contained.localSignature equals method.localSignature) && !contained.isAbstractMethod &&
              areEqual(contained.returnType, method.returnType))
            ret = false
        )
      }
    )
    return ret
  }

  lazy val containedMethodMap: mutable.HashMap[String, Seq[MethodDeclaration]] = {
    var ret = mutable.HashMap.empty[String, Seq[MethodDeclaration]]
    getSuperType(this) match {
      case Some(superType) => {
        inheritedMethodMap ++= superType.inheritedMethodMap
      }
      case None => {}
    }

    this.superInterfaces.foreach(
      superInterface => {
        val interface = getTypeDeclaration(superInterface)
        inheritedMethodMap ++= interface.inheritedMethodMap
      }
    )

    ret ++= inheritedMethodMap
    ret += {fullName(this) -> methodMap.values.toSeq}
    ret
  }

  lazy val inheritMethods: mutable.HashMap[String, Seq[MethodDeclaration]] = {
    var ret = mutable.HashMap.empty[String, Seq[MethodDeclaration]]

    this.supers.foreach(superType => {
        val superTypeContained = superType.containedMethodMap.values.toSeq.flatten.toArray
        val localSigatures = this.methods.map(method => method.localSignature)
        superTypeContained.foreach(contained =>
          if (!localSigatures.contains(contained)) {
            if(!contained.isAbstractMethod) {
              // TODO: Inefficient
              if (!ret.contains(fullName(contained.typeDeclaration)))
                ret += {fullName(contained.typeDeclaration) -> Seq(contained)}
              else
                ret(fullName(contained.typeDeclaration)) = ret(fullName(contained.typeDeclaration)) :+ contained
            } else {
              // All abs
              if (isAllAbstract(contained)) {
                if (!ret.contains(fullName(contained.typeDeclaration)))
                  ret += {fullName(contained.typeDeclaration) -> Seq(contained)}
                else
                  ret(fullName(contained.typeDeclaration)) = ret(fullName(contained.typeDeclaration)) :+ contained
              }
            }
          }
        )
      }
    )

    ret
  }
}

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
        val superTypeContained = superType.containedMethodMap.values.toSeq.flatten.toArray
        superTypeContained.foreach(
          contained =>
            if ((contained.returnTypeLocalSignature equals method.returnTypeLocalSignature) && !contained.isAbstractMethod &&
                areEqual(contained.returnType, method.returnType))
              ret = false
        )
      }
    )
    return ret
  }

<<<<<<< HEAD
  lazy val containedMethodMap: mutable.HashMap[String, IndexedSeq[MethodDeclaration]] = {
    var ret = mutable.HashMap.empty[String, IndexedSeq[MethodDeclaration]]

    ret ++= this.inheritMethods
    ret += {fullName(this) -> methodMap.values.toIndexedSeq}

=======
  lazy val containedMethodMap: mutable.HashMap[String, Seq[MethodDeclaration]] = {
    var ret = mutable.HashMap.empty[String, Seq[MethodDeclaration]]
    getSuperType(this) match {
      case Some(superType) => {
        inheritMethods ++= superType.inheritMethods
      }
      case None => {}
    }

    this.superInterfaces.foreach(
      superInterface => {
        val interface = getTypeDeclaration(superInterface)
        inheritMethods ++= interface.inheritMethods
      }
    )

    ret ++= inheritMethods
    ret += {fullName(this) -> methodMap.values.toSeq}
>>>>>>> 064b2f5a5c984e5eb62ee6939974c7135c55e0de
    ret
  }

  lazy val inheritMethods: mutable.HashMap[String, IndexedSeq[MethodDeclaration]] = {
    var ret = mutable.HashMap.empty[String, IndexedSeq[MethodDeclaration]]

    this.supers.foreach(
      superType => {
<<<<<<< HEAD
        val localSigatures = this.methods.map(method => method.returnTypeLocalSignature)
        val array = superType.containedMethodMap.values.flatten.toArray
        array foreach {
          contained =>
            if (!localSigatures.contains(contained.returnTypeLocalSignature)) {
=======
        val localSigatures = this.methods.map(method => method.localSignature)
        superType.containedMethodMap.values.flatten foreach {
          contained =>
            if (!localSigatures.contains(contained.localSignature)) {
>>>>>>> 064b2f5a5c984e5eb62ee6939974c7135c55e0de
              if (!contained.isAbstractMethod) {
                // TODO: Inefficient
                if (!ret.contains(fullName(contained.typeDeclaration)))
                  ret += {fullName(contained.typeDeclaration) -> mutable.IndexedSeq(contained)}
                else
                  ret(fullName(contained.typeDeclaration)) = ret(fullName(contained.typeDeclaration)) :+ contained
              } else {
                // All abs
                if (isAllAbstract(contained)) {
                  if (!ret.contains(fullName(contained.typeDeclaration)))
                    ret += {fullName(contained.typeDeclaration) -> mutable.IndexedSeq(contained)}
                  else
                    ret(fullName(contained.typeDeclaration)) = ret(fullName(contained.typeDeclaration)) :+ contained
                }
              }
           }
        }
      }
    )

    ret
  }
}

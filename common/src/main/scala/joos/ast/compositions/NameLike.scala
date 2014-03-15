package joos.ast.compositions

import joos.ast.compositions.NameLike._
import joos.ast.types.Type
import joos.ast.declarations.TypeDeclaration

object NameLike {
  type NameClassification = Int
  val Ambiguous = 1
  val PackageName = 2
  val TypeName = 3
  val ExpressionName = 4
  val MethodName = 5
  val PackageOrTypeName = 6
}

trait NameLike {

  /**
   * Gets the standard name separated by .
   */
  def standardName: String

  var classification = Ambiguous

  def classifyContext(newClassification: NameClassification) {
    require(classification == Ambiguous)
    classification = newClassification
  }

  override implicit def toString = standardName

  override def hashCode = standardName.hashCode

  override def equals(that: Any) = that match {
    case that: NameLike => that.standardName == standardName
    case _ => false
  }
}

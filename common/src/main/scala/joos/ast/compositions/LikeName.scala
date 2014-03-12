package joos.ast.compositions

import joos.ast.compositions.LikeName._
import joos.ast.types.Type
import joos.ast.declarations.TypeDeclaration

object LikeName {
  type NameClassification = Int
  val Ambiguous = 1
  val PackageName = 2
  val TypeName = 3
  val ExpressionName = 4
  val MethodName = 5
  val PackageOrTypeName = 6
}

trait LikeName {

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
    case that: LikeName => that.standardName == standardName
    case _ => false
  }
}

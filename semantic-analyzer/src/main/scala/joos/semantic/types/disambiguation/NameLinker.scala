package joos.semantic.types.disambiguation

import joos.ast.NameClassification._
import joos.ast.expressions.{NameExpression, SimpleNameExpression, QualifiedNameExpression}
import joos.ast.visitor.AbstractSyntaxTreeVisitorBuilder
import joos.ast.{AbstractSyntaxTreeVisitorWithEnvironment, CompilationUnit}
import joos.ast.declarations.TypeDeclaration

/**
 * Links names to its declarations
 */
class NameLinker(implicit val unit: CompilationUnit) extends AbstractSyntaxTreeVisitorWithEnvironment {
//  override def apply(name: QualifiedNameExpression) {
//    name.qualifier.accept(this)
//    name.classification match {
//      case PackageName =>
//      case TypeName =>
//        unit.getVisibleType(name) match {
//          case Some(tipe) => name.declarationLink.link(tipe)
//          case None => throw new AmbiguousNameException(name)
//        }
//      case ExpressionName =>
//        name.qualifier.declarationLink() match {
//          case tipe: TypeDeclaration =>
//            tipe.containedFields.get(name.name) match {
//              case Some(variable) =>
//                name.declarationLink.link(variable)
//                name.name.declarationLink.link(variable)
//              case None => throw new AmbiguousNameException(name.name)
//            }
//        }
//      case MethodName =>
//    }
//  }
//
//  override def apply(name: SimpleNameExpression) {
//    // There shouldn't be any ambiguous names at this stage
//    name.classification match {
//      case PackageName =>
//      // TODO: Add package declaration
//      case TypeName =>
//        unit.getVisibleType(name) match {
//          case Some(tipe) => name.declarationLink.link(tipe)
//          case None => throw new AmbiguousNameException(name)
//        }
//      case ExpressionName =>
//        block.getVariable(name) match {
//          case Some(variable) => name.declarationLink.link(variable)
//          case None => throw new AmbiguousNameException(name)
//        }
//      case MethodName =>
//    }
//  }
}

object NameLinker extends AbstractSyntaxTreeVisitorBuilder[NameLinker] {
  override def build(implicit unit: CompilationUnit) = new NameLinker
}

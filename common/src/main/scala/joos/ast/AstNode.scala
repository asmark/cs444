package joos.ast

import joos.ast.declarations._
import joos.ast.expressions._
import joos.ast.statements._
import joos.ast.visitor.AstVisitor

trait AstNode {
  def accept(visitor: AstVisitor) {
    this match {
      case node: FieldDeclaration => visitor(node)
//      case node: ImportDeclaration => visitor(node)
      case node: MethodDeclaration => visitor(node)
//      case node: ModuleDeclaration => visitor(node)
//      case node: PackageDeclaration => visitor(node)
      case node: SingleVariableDeclaration => visitor(node)
      case node: TypeDeclaration => visitor(node)
      case node: ArrayAccessExpression => visitor(node)
      case node: ArrayCreationExpression => visitor(node)
//      case node: BooleanLiteral => visitor(node)
      case node: CastExpression => visitor(node)
//      case node: CharacterLiteral => visitor(node)
      case node: ClassCreationExpression => visitor(node)
      case node: FieldAccessExpression => visitor(node)
      case node: InfixExpression => visitor(node)
      case node: InstanceOfExpression => visitor(node)
      //      case node: IntegerLiteral => visitor(node)
      case node: MethodInvocationExpression => visitor(node)
      //      case node: NullLiteral => visitor(node)
      case node: ParenthesizedExpression => visitor(node)
      case node: PrefixExpression => visitor(node)
      case node: QualifiedNameExpression => visitor(node)
      case node: SimpleNameExpression => visitor(node)
      //      case node: StringLiteral => visitor(node)
      //      case node: ThisExpression => visitor(node)
      case node: VariableDeclarationExpression => visitor(node)
      case node: VariableDeclarationFragment => visitor(node)
      case node: Block => visitor(node)
      case node: ExpressionStatement => visitor(node)
      case node: ForStatement => visitor(node)
      case node: IfStatement => visitor(node)
      case node: ReturnStatement => visitor(node)
      case node: WhileStatement => visitor(node)
      case node: CompilationUnit => visitor(node)
      case node =>
        //Logger.logInformation(s"Uncaptured visitor ${node}")
    }
  }
}

package joos.ast

import joos.ast.compositions.BlockLike
import joos.ast.declarations._
import joos.ast.expressions._
import joos.ast.statements._
import joos.ast.visitor.AstCompleteVisitor
import joos.semantic.BlockEnvironment

trait AbstractSyntaxTreeVisitorWithEnvironment extends AstCompleteVisitor {
  def unit: CompilationUnit

  private[this] var _tipe: TypeDeclaration = _
  private[this] var _method: MethodDeclaration = _
  private[this] var _block: BlockEnvironment = _

  protected def tipe = {
    require(_tipe != null)
    _tipe
  }

  protected def block = {
    require(_block != null)
    _block
  }

  protected def method = {
    require(_method != null)
    _method
  }

  override def apply(tipe: TypeDeclaration) {
    use(tipe) {
      tipe =>
        _tipe = tipe
        super.apply(tipe)
    }
  }

  override def apply(method: MethodDeclaration) {
    use(method) {
      method =>
        _method = method
        super.apply(method)
    }
  }

  override def apply(statement: ForStatement) {
    use(statement)(super.apply)
  }

  override def apply(statement: WhileStatement) {
    use(statement)(super.apply)
  }

  override def apply(block: Block) {
    use(block)(super.apply)
  }

  override def apply(statement: IfStatement) {
    use(statement)(_.condition.accept(this))
    use(statement)(_.trueStatement.accept(this))
    use(statement)(_.falseStatement.foreach(_.accept(this)))
  }

  override def apply(invocation: MethodInvocationExpression) {
    invocation.expression.foreach(_.accept(this))
    invocation.arguments.foreach(_.accept(this))
  }

  override def apply(expression: VariableDeclarationExpression) {
    _block = expression.blockEnvironment
    super.apply(expression)
  }

  private[this] def use[T <: BlockLike](block: T)(f: T => Unit) {
    val environment = _block
    _block = block.blockEnvironment
    f(block)
    _block = environment
  }
}

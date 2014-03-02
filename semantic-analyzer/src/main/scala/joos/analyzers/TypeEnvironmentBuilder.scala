package joos.analyzers

import joos.ast.declarations._
import joos.ast.{CompilationUnit, AstVisitor}

class TypeEnvironmentBuilder(implicit module: ModuleDeclaration) extends AstVisitor {
  private[this] implicit var typed: TypeDeclaration = null
  private[this] var unit: CompilationUnit = null
  private[this] var packaged: PackageDeclaration = null

  override def apply(unit: CompilationUnit) {
    this.unit = unit
    packaged = unit.packageDeclaration
    unit.typeDeclaration.map(_.accept(this))
  }

  override def apply(typed: TypeDeclaration) {
    this.typed = typed
    typed.methods.foreach(_.accept(this))
    typed.fields.foreach(_.accept(this))
  }

  override def apply(method: MethodDeclaration) {
    typed.add(method)
  }

  override def apply(field: FieldDeclaration) {
    typed.add(field)
  }
}

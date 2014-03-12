package joos.semantic.types.disambiguation

import joos.ast.CompilationUnit
import joos.ast.compositions.LikeName._
import joos.ast.declarations.MethodDeclaration
import joos.ast.expressions.{VariableDeclarationExpression, QualifiedNameExpression, SimpleNameExpression}
import joos.ast.visitor.AstCompleteVisitor
import joos.semantic.{BlockEnvironment, TypeEnvironment}
import joos.ast.statements._

class NameDisambiguator(implicit unit: CompilationUnit) extends AstCompleteVisitor {
  private var typeEnvironment: TypeEnvironment = null
  private var blockEnvironment: BlockEnvironment = null

  override def apply(unit: CompilationUnit) {
    typeEnvironment = unit.typeDeclaration.getOrElse(null)

    super.apply(unit)
  }

  override def apply(methodDeclaration: MethodDeclaration) {
    val oldBlock = blockEnvironment
    blockEnvironment = methodDeclaration.environment
    super.apply(methodDeclaration)
    blockEnvironment = oldBlock
  }

  override def apply(block: Block) {
    val oldBlock = blockEnvironment
    blockEnvironment = block.environment
    block.statements foreach (_.accept(this))
    blockEnvironment = oldBlock
  }

  override def apply(statement: IfStatement) {
    val oldBlock = blockEnvironment
    blockEnvironment = statement.environment
    statement.condition.accept(this)
    blockEnvironment = statement.environment
    statement.trueStatement.accept(this)
    blockEnvironment = statement.environment
    statement.falseStatement.map(_.accept(this))
    blockEnvironment = oldBlock
  }

  override def apply(statement: ExpressionStatement) {
    val oldBlock = blockEnvironment
    blockEnvironment = statement.environment
    statement.expression.accept(this)
    blockEnvironment = oldBlock
  }

  override def apply(statement: WhileStatement) {
    val oldBlock = blockEnvironment
    blockEnvironment = statement.environment
    statement.condition.accept(this)
    statement.body.accept(this)
    blockEnvironment = oldBlock
  }

  override def apply(statement: ForStatement) {
    val oldBlock = blockEnvironment
    blockEnvironment = statement.environment
    statement.initialization.map(_.accept(this))
    statement.condition.map(_.accept(this))
    statement.update.map(_.accept(this))
    statement.body.accept(this)
    blockEnvironment = oldBlock
  }

  override def apply(statement: ReturnStatement) {
    val oldBlock = blockEnvironment
    blockEnvironment = statement.environment
    statement.expression.map(_.accept(this))
    blockEnvironment = oldBlock
  }

  override def apply(expression: VariableDeclarationExpression) {
    blockEnvironment = expression.environment
    expression.declaration.accept(this)
  }

  // If the AmbiguousName is a simple name, consisting of a single Identifier:
  override def apply(simpleName: SimpleNameExpression) {
    if (simpleName.classification != Ambiguous) {
      return
    }
    require(typeEnvironment != null)
    // If the Identifier appears within the scope (§6.3) of a local variable declaration (§14.4) or parameter declaration (§8.4.1, §8.8.1,
    // §14.19) or field declaration (§8.3) with that name, then the AmbiguousName is reclassified as an ExpressionName.
    if (blockEnvironment != null && blockEnvironment.getVariable(simpleName).isDefined) {
        simpleName.classifyContext(ExpressionName)

      // Otherwise, if the Identifier appears within the scope (§6.3) of a local class declaration (§14.3) or member type declaration (§8.5,
      // §9.5) with that name, then the AmbiguousName is reclassified as a TypeName.
    } else if (typeEnvironment.containedFields.contains(simpleName) ||
        typeEnvironment.containedMethods.contains(simpleName)) {
      simpleName.classifyContext(ExpressionName)
      // Otherwise, if a type of that name is declared in the compilation unit (§7.3) containing the Identifier,
      // either by a single-type-import declaration (§7.5.1) or by a top level class (§8) or interface type declaration (§9),
      // then the AmbiguousName is reclassified as a TypeName.
      // Otherwise, if a type of that name is declared in another compilation unit (§7.3) of the package (§7.1) of the compilation unit containing
      // the Identifier, then the AmbiguousName is reclassified as a TypeName.
      // Otherwise, if a type of that name is declared by exactly one type-import-on-demand declaration (§7.5.2) of the compilation unit containing
      // the Identifier, then the AmbiguousName is reclassified as a TypeName.
    } else if (unit.getVisibleType(simpleName).isDefined) {
      simpleName.classifyContext(TypeName)
      // Otherwise, the AmbiguousName is reclassified as a PackageName. A later step determines whether or not a package of that name actually exists.
    } else {
      simpleName.classifyContext(PackageName)
    }
    super.apply(simpleName)
  }

  override def apply(qualifiedName: QualifiedNameExpression) {
    // If the AmbiguousName is a qualified name, consisting of a name, a ".", and an Identifier, then the name to the left of the "." is first
    // reclassified, for it is itself an AmbiguousName. There is then a choice:
    qualifiedName match {
      case QualifiedNameExpression(q@QualifiedNameExpression(_, _), SimpleNameExpression(_)) => {
        apply(q)
      }
      case QualifiedNameExpression(s1@SimpleNameExpression(_), SimpleNameExpression(_)) => {
        apply(s1)
      }
    }
    if (qualifiedName.classification != Ambiguous) {
      return
    }
    val leftName = qualifiedName.qualifier
    val rightName = qualifiedName.name
    // If the name to the left of the "." is reclassified as a PackageName, then if there is a package whose name is the name to the left of the "
    // ." and that package contains a declaration of a type whose name is the same as the Identifier, then this AmbiguousName is reclassified as a
    // TypeName. Otherwise, this AmbiguousName is reclassified as a PackageName. A later step determines whether or not a package of that name
    // actually exists.
    if (leftName.classification == PackageName) {
      if (unit.getVisibleType(qualifiedName).isDefined) {
        qualifiedName.classifyContext(TypeName)
        rightName.classifyContext(TypeName)
      } else {
        qualifiedName.classifyContext(PackageName)
        rightName.classifyContext(PackageName)
      }
      // If the name to the left of the "." is reclassified as a TypeName, then if the Identifier is the name of a method or field of the class or
      // interface denoted by TypeName, this AmbiguousName is reclassified as an ExpressionName. Otherwise,
      // if the Identifier is the name of a member type of the class or interface denoted by TypeName,
      // this AmbiguousName is reclassified as a TypeName. Otherwise, a compile-time error results.
    } else if (leftName.classification == TypeName) {
      val referencedType = unit.getVisibleType(leftName)
      if (referencedType.isEmpty) {
        throw new AmbiguousNameException(qualifiedName)
      }

      if (referencedType.get.containedFields.contains(rightName) || referencedType.get.containedMethods.contains(rightName)) {
        qualifiedName.classifyContext(ExpressionName)
        rightName.classifyContext(ExpressionName)

      } else {
        throw new AmbiguousNameException(qualifiedName)
      }
      // From 1.0 Spec:
      // If the name to the left of the "." is reclassified as an ExpressionName, then this AmbiguousName is reclassified as an ExpressionName.
    } else if (leftName.classification == ExpressionName) {
      qualifiedName.classifyContext(ExpressionName)
      rightName.classifyContext(ExpressionName)
    }

  }
}

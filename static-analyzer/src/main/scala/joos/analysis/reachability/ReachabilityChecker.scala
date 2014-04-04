package joos.analysis.reachability

import joos.analysis.exceptions.MissingReturnException
import joos.ast.CompilationUnit
import joos.ast.types.PrimitiveType._
import joos.ast.visitor.CompilationUnitUserBuilder
import joos.core.TernaryBoolean._
import joos.ast.types._

class ReachabilityChecker(unit: CompilationUnit) extends (() => Unit) {
  override def apply() {
    unit.typeDeclaration.foreach {
      tipe =>
        for (method <- tipe.methods) {
          method.body.foreach {
            block =>
              val reachableBlock: Reachable = block
              reachableBlock.canStart = Maybe
              reachableBlock.verify()

              method.returnType match {
                case None | Some(VoidType) =>
                case _ =>
                  if (reachableBlock.canFinish == Maybe)
                    throw new MissingReturnException(method)
              }
          }
        }
    }
  }
}

object ReachabilityChecker extends CompilationUnitUserBuilder[ReachabilityChecker] {
  override def build(unit: CompilationUnit) = new ReachabilityChecker(unit)
}

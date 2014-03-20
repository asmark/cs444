package joos.analysis.reachability

import joos.analysis.exceptions.MissingReturnException
import joos.ast.CompilationUnit
import joos.ast.visitor.CompilationUnitUserBuilder
import joos.core.TernaryBoolean._
import joos.ast.types.PrimitiveType._

class ReachabilityVerifier(unit: CompilationUnit) extends (() => Unit) {
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

object ReachabilityVerifier extends CompilationUnitUserBuilder[ReachabilityVerifier] {
  override def build(unit: CompilationUnit) = new ReachabilityVerifier(unit)
}

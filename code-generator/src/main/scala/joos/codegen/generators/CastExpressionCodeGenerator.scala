package joos.codegen.generators

import joos.ast.expressions.CastExpression
import joos.ast.types._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.core.Logger
import joos.assemgen._
import joos.assemgen.Register._
import joos.semantic.types.TypeCheckingException

class CastExpressionCodeGenerator(castExpression: CastExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  // CastExpression := (leftType) rightType
  override def generate() {
    castExpression.expression.expressionType match {

      case SimpleType(_) | ArrayType(_,_) => {
        castExpression.castType match {
          case ArrayType(leftElementType, _) => {
            val subtypeIndex = leftElementType match {
              case leftSimpleType: SimpleType => {
                require(leftSimpleType.declaration != null)
                environment.staticDataManager.getArrayTypeIndex(leftSimpleType.declaration)
              }
              case leftPrimitiveType: PrimitiveType => {
                environment.staticDataManager.getArrayTypeIndex(leftPrimitiveType)
              }
            }

            generateObjectCheck(subtypeIndex)
          }
          case rightType: SimpleType => {
            // Castable to Object, Cloneable and Serializable.
            require(rightType.declaration != null)
            val subtypeIndex = environment.staticDataManager.getTypeIndex(rightType.declaration)
            generateObjectCheck(subtypeIndex)
          }
          case _:PrimitiveType => {
            // TODO: nothing??
          }
//          case NullType => {
//            // TODO: this should go into static type check
//            throw new TypeCheckingException(
//              "instanceof",
//              s"${castExpression.castType.standardName} are not related types ${castExpression.expression.expressionType.standardName}")
//          }
        }
      }

      case _ =>
        Logger.logWarning(s"No Support for ${castExpression.expression.expressionType} in instanceof checks in ${castExpression}")
    }

//    castExpression.expression.generate()
//
//    castExpression.expression.expressionType match {
//      case rightType: SimpleType => {
//
//        castExpression.castType match {
//          case leftType : SimpleType => {
//            val subtypeIndex = environment.staticDataManager.getTypeIndex(leftType.declaration)
//            val endLabel = nextLabel("is_null_object")
//            appendText(
//              :#(s"[BEGIN] Cast Expression ${castExpression}"),
//              cmp(Eax, 0),
//              je(endLabel),
//              mov(Ebx, at(Eax+SubtypeTableOffset)) :# "Move subtype table of left into ebx",
//              mov(Ebx, at(Ebx + 4 * subtypeIndex)) :# "Look up value in subtype table for instance check",
//              cmp(Ebx, 0) :# "Throw exception if it cannot be cast",
//              je(exceptionLabel),
//              endLabel::,
//              :#(s"[END] Cast Expression ${castExpression}")
//            )
//          }
//
//          case ArrayType(rightElementType, _) => {
//            val subtypeIndex = rightElementType match {
//              case rightSimpleType: SimpleType => {
//                environment.staticDataManager.getArrayTypeIndex(rightSimpleType.declaration)
//              }
//              case rightPrimitiveType: PrimitiveType => {
//                environment.staticDataManager.getArrayTypeIndex(rightPrimitiveType)
//              }
//            }
//
//            generateObjectCheck(subtypeIndex)
//          }
//
//          case leftType : PrimitiveType => {
//            // Not sure if we need this
//            appendText(je(exceptionLabel))
//            Logger.logInformation(s"Exception when casting object type to primitive type in ${castExpression}")
//          }
//
//          case NullType => {}
//
//          case x => {
//            Logger.logWarning(s"No support to cast ${rightType} as ${x} in ${castExpression}")
//          }
//
//        }
//
//      }
//
//      // We don't need runtime processing as no narrowing is allowed for primitive
//      case rightType: PrimitiveType => {
//
//        castExpression.castType match {
//          case rightType: PrimitiveType => {
//
//          }
//          case _ => {
//
//          }
//        }
//      }
//
//      case rightType: ArrayType => {
//
//        castExpression.castType match {
//          case ArrayType(rightElementType, _) => {
//            val subtypeIndex = rightElementType match {
//              case rightSimpleType: SimpleType => {
//                environment.staticDataManager.getArrayTypeIndex(rightSimpleType.declaration)
//              }
//              case rightPrimitiveType: PrimitiveType => {
//                environment.staticDataManager.getArrayTypeIndex(rightPrimitiveType)
//              }
//            }
//
//            generateObjectCheck(subtypeIndex)
//          }
//          case leftType: SimpleType => {
//            // Castable to Object, Cloneable and Serializable.
//            val subtypeIndex = environment.staticDataManager.getTypeIndex(leftType.declaration)
//            generateObjectCheck(subtypeIndex)
//          }
//          case x =>
//            Logger.logWarning(s"No support to cast ${x} in ${castExpression}")
//        }
//      }
//
//      case NullType => {}
//
//      case x =>
//        Logger.logWarning(s"No support to cast ${x} in ${castExpression}")
//    }
  }

  private def generateObjectCheck(subtypeIndex: Int) {
    appendText(
      :#(s"[BEGIN] Cast check ${castExpression}"),
      :#("Look up left hand side"),
      #>
    )

    castExpression.expression.generate()

    val endLabel = nextLabel("is_null_object")
    appendText(
      #<,
      emptyLine,
      cmp(Eax, 0),
      je(endLabel),
      mov(Ebx, at(Eax + SubtypeTableOffset)) :# "Put address of subtype table in eax",
      mov(Ebx, at(Ebx + 4 * subtypeIndex)) :# "Look up value in subtype table for instance check",
      cmp(Ebx, 0) :# "Throw exception if it cannot be cast",
      je(exceptionLabel),
      endLabel::,
      :#(s"[END] Cast check ${castExpression}")
    )
  }
}
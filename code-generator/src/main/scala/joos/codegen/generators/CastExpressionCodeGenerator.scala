package joos.codegen.generators

import joos.ast.expressions.CastExpression
import joos.ast.types._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.core.Logger
import joos.assemgen._
import joos.assemgen.Register._

class CastExpressionCodeGenerator(expression: CastExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    expression.expression.generate()

    expression.expression.expressionType match {
      case leftType: SimpleType => {

        expression.castType match {
          case rightType : SimpleType => {
            val subtypeIndex = environment.staticDataManager.getTypeIndex(rightType.declaration)
            appendText(
              :#(s"[BEGIN] Cast Expression ${expression}"),
              mov(Ebx, at(Eax+SubtypeTableOffset)) :# "Move subtype table of left into ebx",
              mov(Ebx, at(Ebx + 4 * subtypeIndex)) :# "Look up value in subtype table for instance check",
              cmp(Ebx, 0) :# "Throw exception if it cannot be cast",
              je(exceptionLabel)
              :#(s"[END] Cast Expression ${expression}")
            )
          }

          case rightType : PrimitiveType => {
            // Not sure if we need this
            appendText(je(exceptionLabel))
            Logger.logInformation(s"Exception when casting object type to primitive type in ${expression}")
          }

          case x => {
            Logger.logWarning(s"No support to cast ${leftType} as ${x} in ${expression}")
          }

        }

      }

      case leftType: PrimitiveType => {

        expression.castType match {
          case rightType: PrimitiveType => {

          }
          case _ => {

          }
        }


      }

      case x =>
        Logger.logWarning(s"No support to cast ${x} in ${expression}")
    }



    //    // EAX should have a reference to the object/class etc
    //
    //    expression.castType match {
    //      case PrimitiveType.ByteType => {
    //        expression.castType match {
    //          case PrimitiveType.ShortType | PrimitiveType.IntegerType=> {
    //            appendText(:#("[BEGIN] Casting to Byte"))
    //            prologue(0)
    //            appendText(
    //              mov(Ebx, toExpression(255)),
    //              and(Eax, Ebx)
    //            )
    //            epilogue(0)
    //            appendText(:#("[END] Casting to Byte"))
    //          }
    //          case _ =>
    //        }
    //      }
    //      case PrimitiveType.ShortType => {
    //        expression.castType match {
    //          case PrimitiveType.IntegerType => {
    //            appendText(:#("[BEGIN] Casting Int to Short"))
    //            prologue(0)
    //            appendText(
    //              mov(Ebx, toExpression(255)),
    //              and(Eax, Ebx)
    //            )
    //            epilogue(0)
    //            appendText(:#("[END] Casting Int to Short"))
    //          }
    //          case _ => {}
    //        }
    //      }
    //      case PrimitiveType.CharType => {} // Place holder
    //      case dst: SimpleType => {
    //        expression.expression.expressionType match {
    //          case ArrayType(_,_) => {
    //            require(dst.declaration.fullName == javaLangObject.standardName ||
    //                dst.declaration.fullName == javaLangObject.standardName ||
    //                dst.declaration.fullName == javaIOSerializable.standardName)
    //          }
    //          case NullType => {
    //            //TODO: Do nothing?
    //
    //          }
    //          case src: SimpleType => {
    //            val dstTypeDeclration = environment.typeEnvironment.compilationUnit.getVisibleType(dst.name)
    //            require(dstTypeDeclration.isDefined)
    //
    //            val dstSit = selectorTableLabel(dstTypeDeclration.get)
    //            val dstSubTypeTable = subtypeTableLabel(dstTypeDeclration.get)
    //            val dstTypeOffset = environment.staticDataManager.getTypeIndex(dstTypeDeclration.get) * 4
    //
    //            val validLabel = "valid" + DefaultUniqueIdGenerator.nextId()
    //            val endLabel = "end" + DefaultUniqueIdGenerator.nextId()
    //
    //            appendText(:#(s"[BEGIN] Casting ${src.standardName} to ${dst.standardName}"))
    //            prologue(0)
    //            appendText(
    //              #>,
    //              push(Eax) :#"Save EAX",
    //              mov(Ebx, Eax),
    //              add(Ebx, toExpression(SelectorTableOffset)),
    //              mov(Ebx, at(Ebx)) :#"EBX should point to the sub type table",
    //              add(Ebx, dstTypeOffset),
    //              mov(Ebx, at(Ebx)) :#"Look up value in the subtype table",
    //              cmp(Ebx, toExpression(1)),
    //              je(labelReference(validLabel)),
    //              call(labelReference(exceptionLabel)) :#"Cast error",
    //              jmp(labelReference(endLabel)),
    //              validLabel ::,
    //              mov(Ebx, Eax) :#"Update selector table",
    //              add(Ebx, toExpression(SelectorTableOffset)),
    //              movdw(at(Ebx), labelReference(dstSit)),
    //              add(Ebx, toExpression(SubtypeTableOffset - SelectorTableOffset)) :#"Update sub type table",
    //              movdw(at(Ebx), labelReference(dstSubTypeTable)),
    //              endLabel ::,
    //              pop(Eax) :#"Restore EAX",
    //              #<
    //            )
    //            epilogue(0)
    //            appendText(:#(s"[END] Casting Casting ${src.standardName} to ${dst.standardName}"))
    //          }
    //          case _ => {}
    //        }
    //      }
    //      case ArrayType(_,_) => {
    //
    //      }
    //      case _ => {}
    //    }
  }

}
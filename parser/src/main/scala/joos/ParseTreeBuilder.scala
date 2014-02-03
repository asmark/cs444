package joos

import joos.tokens.TokenKind.TokenKind
import scala.collection.mutable
import joos.parsetree.{LeafNode, ParseTree}

class ParseTreeBuilder {

}

sealed private case class Transition(val state : Int, val symbol: String)

object ParseTreeBuilder {

  val actionTable = LrOneReader(null)

  def parse(terminals : Seq[String]) {


    val nodeStack = mutable.Stack(LeafNode("BOF"))
    val stateStack = mutable.Stack(Transition(0, "BOF"))

//    terminals.foreach { terminal =>
//      while ({
//        actionTable.getAction(stateStack.top.state, terminal) match {
//
//        }
//      })
//    }


  }

}

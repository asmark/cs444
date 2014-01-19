package joos.lexer

/**
 * Created by freddie on 18/01/14.
 */
abstract class RegExp {
  protected var _entranceAtom: Atom;
  protected var _exitAtom: Atom;

  def entrance = _entranceAtom
  def entrance_= (_entranceAtom: Atom) = this._entranceAtom = _entranceAtom

  def exit = _exitAtom
  def exit_= (_exitAtom: Atom) = this._exitAtom = _exitAtom
}

class Atom(var _node: NFANode) extends RegExp {
  protected var _entranceAtom: Atom = this
  protected var _exitAtom: Atom = this

  def node = _node
  def node_= (_node: NFANode) = this._node = _node

  def addTransition(char: Char, dst: Atom) = {
    this._node.addTransition(char, dst._node)
  }
}

class Concatenation() extends RegExp {
  protected var _entranceAtom: Atom = _
  protected var _exitAtom: Atom = _

  def build(inputs: Array[RegExp]): RegExp = {
    assert(inputs.length > 1)
    this._entranceAtom = inputs(0).entrance
    this._exitAtom = inputs(inputs.length - 1).exit
    var idx = 0
    for (idx <- 0 to inputs.length - 2) {
      inputs(idx).exit.addTransition(NFANode.Epsilon, inputs(idx + 1).entrance);
    }
    this
  }
}

class Alternation() extends RegExp {
  protected var _entranceAtom: Atom = _
  protected var _exitAtom: Atom = _

  def build(inputs: Array[RegExp]): RegExp = {
    assert(inputs.length > 1)
    this._entranceAtom = new Atom(new NonAcceptingNFANode)
    this._exitAtom = new Atom(new NonAcceptingNFANode)
    var idx = 0
    for (idx <- 0 to inputs.length - 2) {
      this._entranceAtom.addTransition(NFANode.Epsilon, inputs(idx).entrance)
      inputs(idx).exit.addTransition(NFANode.Epsilon, this._exitAtom)
    }
    this
  }
}

class Closure() extends RegExp {
  protected var _entranceAtom: Atom = _
  protected var _exitAtom: Atom = _

  def build(atom: RegExp): RegExp = {
    this._entranceAtom = new Atom(new NonAcceptingNFANode)
    this._entranceAtom.addTransition(NFANode.Epsilon, atom.entrance)
    this._exitAtom = new Atom(new NonAcceptingNFANode)
    atom.exit.addTransition(NFANode.Epsilon, this._exitAtom)
    this._exitAtom.addTransition(NFANode.Epsilon, atom.entrance)
    this
  }
}
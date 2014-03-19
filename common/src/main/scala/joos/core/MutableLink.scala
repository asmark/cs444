package joos.core


/**
 * Represents a link to some target of type {{T}}
 */
class MutableLink[T] {
  private[this] var target: Option[T] = None

  /**
   * Returns the target
   */
  def apply(): T = target.get

  /**
   * Links to the given {{target}}
   * It will overwrite the old target
   */
  def link(target: T): this.type = {
    this.target = Some(target)
    this
  }
}

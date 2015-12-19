package arx.core.traits

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/7/13
 * Time: 4:19 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._


/**
 * Has a parent/child relationship that should be kept bidirectionally
 * accurate
 */
trait TFamilial[T <: TFamilial[T]] extends AnyRef {
	protected var _parent : Option[T] = None
	protected var _children : List[T] = Nil

	def parent = _parent
	def children = _children
	def parent_= ( p : T ) {
		p.parent match {
			case Some(existingParent) => existingParent._children = existingParent._children without this
			case _ =>
		}

		if ( p eq null ) {
			_parent = None
		} else {
			_parent = Some(p)
		}

		_parent match {
			case Some(newParent) => newParent._children ::= this.asInstanceOf[T]
			case _ =>
		}
	}

}
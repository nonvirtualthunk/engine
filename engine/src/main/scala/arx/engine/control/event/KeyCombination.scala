package arx.engine.control.event.Event

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/17/15
 * Time: 2:42 PM
 */

import arx.Prelude._
import scalaxy.loops._

case class KeyCombination(key: Int, modifiers: KeyModifiers = KeyModifiers.None) {
	def active: Boolean = {
		KeyboardMirror.isKeyDown(key) && KeyboardMirror.isActive(modifiers)
	}

	def unapply(a: (Int, KeyModifiers)): Option[KeyCombination] = {
		if (a._1.equals(key) && modifiers == a._2) {Some(this) } else {
			None
		}
	}

	def unapply(a: Int): Option[KeyCombination] = {
		if (a.equals(key) && modifiers == KeyModifiers.None) {Some(this) } else {
			None
		}
	}

	override def toString = s"KeyCombination($key, $modifiers)"
}

object KeyCombination {

	val Sentinel = new KeyCombination(0, KeyModifiers.None)
}
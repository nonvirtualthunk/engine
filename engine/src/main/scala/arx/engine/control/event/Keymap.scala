package arx.engine.control.event

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/17/15
 * Time: 2:52 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.datastructures.MultiMap
import scala.collection.mutable
import scalaxy.loops._

object Keymap {
	protected val mapping = new MultiMap[KeyCombination, String]()
	protected val reverseMapping = new mutable.HashMap[String, KeyCombination]()

	def register (identifier : String, keyCombination: KeyCombination): Unit = synchronized {
		if (!reverseMapping.contains(identifier)) {
			mapping.add(keyCombination,identifier)
			reverseMapping.put(identifier,keyCombination)
		} else {
			Noto.error(s"Attempting to register duplicate keymapping: $keyCombination -> $identifier")
		}
	}
	def register (identifier : String, keyCode : Int): Unit = {
		register(identifier, KeyCombination(keyCode))
	}
	def register (identifier : String, keyCode : Int, keyModifiers: KeyModifiers) : Unit = {
		register(identifier, KeyCombination(keyCode, keyModifiers))
	}

	def unregister(identifier : String): Unit = synchronized {
		mapping.intern.foreach {
			case (_, identifiers) =>
				identifiers.indexOf(identifier) match {
					case -1 => // do nothing
					case i => identifiers.remove(i)
				}
		}
		reverseMapping.remove(identifier)
	}

	def mappingFor(keyCombination : KeyCombination) = {
		mapping.get(keyCombination).headOption
	}
	def mappingFor(keyPress : KeyPressEvent) = {
		mapping.get(KeyCombination(keyPress.key, keyPress.modifiers)).headOption
	}
	def mappingActive(identifier : String) = {
		reverseMapping(identifier).active
	}
}

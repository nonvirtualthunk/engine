package arx.engine.control.event.Event

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
	protected val mapping = new MultiMap[KeyCombination, Identifier]()
	protected val reverseMapping = new mutable.HashMap[Identifier, KeyCombination]()

	def register (namespace : String, identifier : String, keyCombination: KeyCombination): Unit = synchronized {
		if (!reverseMapping.contains(Identifier(namespace,identifier))) {
			mapping.add(keyCombination,Identifier(namespace,identifier))
			reverseMapping.put(Identifier(namespace,identifier),keyCombination)
		} else {
			Noto.error(s"Attempting to register duplicate keymapping: $keyCombination -> $identifier")
		}
	}
	def register (namespace : String, identifier : String, keyCode : Int): Unit = {
		register(namespace, identifier, KeyCombination(keyCode))
	}
	def register (namespace : String, identifier : String, keyCode : Int, keyModifiers: KeyModifiers) : Unit = {
		register(namespace, identifier, KeyCombination(keyCode, keyModifiers))
	}

	def unregister(namespace : String, identifier : String): Unit = synchronized {
		mapping.intern.foreach {
			case (_, identifiers) =>
				identifiers.indexOf(identifier) match {
					case -1 => // do nothing
					case i => identifiers.remove(i)
				}
		}
		reverseMapping.remove(Identifier(namespace,identifier))
	}

	def mappingFor(keyCombination : KeyCombination) : Option[Identifier] = {
		mapping.get(keyCombination).headOption
	}
	def mappingFor(keyCombination : KeyCombination, namespace : String) : Option[String] = {
		mapping.get(keyCombination).find(ident => ident.namespace == namespace).map(_.identifier)
	}
	def mappingFor(keyPress : KeyPressEvent) : Option[Identifier] = {
		mappingFor(KeyCombination(keyPress.key, keyPress.modifiers))
	}
	def mappingFor(keyPress : KeyPressEvent, namespace : String) : Option[String] = {
		mappingFor(KeyCombination(keyPress.key, keyPress.modifiers), namespace)
	}
	def mappingActive(namespace : String, identifier : String) : Boolean = {
		reverseMapping.get(Identifier(namespace, identifier)).exists(_.active)
	}

	case class Identifier (namespace : String, identifier : String)
}

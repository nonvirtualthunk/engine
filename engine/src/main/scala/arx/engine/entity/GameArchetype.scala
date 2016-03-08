package arx.engine.entity

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 3/5/16
  * Time: 8:26 AM
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.representation.ConfigAssistant
import arx.core.representation.ConfigValue
import arx.core.traits.TIdentifiable
import arx.core.traits.TSentinel

import scala.collection.mutable
import scalaxy.loops._
import arx.core.vec._

class GameArchetype(nomen: String, val kind: String) extends GameEntity(nomen) with TIdentifiable {
	protected val baseIdentifier = kind + "-" + name
	def identifier = baseIdentifier
	var displayName = name

	var description = ""

	override def hashCode(): Int = identifier.hashCode
	override def equals(obj: Any): Boolean = obj match {
		case arch: GameArchetype => arch.identifier == this.identifier
		case _ => false
	}
	override def toString: String = {
		nomen + "(" + kind + ")"
	}
}

object GameArchetype {
	val Sentinel: GameArchetype = new GameArchetype("Sentinel", "Sentinel") with TSentinel {

	}

	protected val _archetypes = new mutable.HashMap[CaseInsensitiveString, Map[String, GameArchetype]]
	def archetype(kind: String, name: String) = _archetypes.get(kind).flatMap(m => m.get(name.toLowerCase)).getOrElse(Sentinel)
	def archetypes(kind: String) = _archetypes.getOrElse(kind, Map())

	def addArchetype(arch: GameArchetype): Unit = {
		val existing = _archetypes.getOrElse(arch.kind, Map())
		_archetypes.put(arch.kind, existing + (arch.name.toLowerCase() -> arch))
	}


	def loadAllArchetypes(baseConfLocation: String, leafField: String, constr: (String, ConfigValue) => GameArchetype): Unit = {
		val byPackage = ConfigAssistant.loadAllConfigsByPackage(baseConfLocation, leafField)

		byPackage.foreach{ case (name, obj) =>
			val newArch = constr(name.toString, obj)
			addArchetype(newArch)
		}
	}
}
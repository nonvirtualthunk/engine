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
import arx.core.traits.TSentinelable

import scala.collection.mutable
import scalaxy.loops._
import arx.core.vec._

class GameArchetype(nomen: String, val kind: TArchetypeKind) extends GameEntity(nomen) with TIdentifiable with TSentinelable {
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
		kind + "(" + nomen+ ")"
	}


	def create() : TGameEntity = {
		val e = new GameEntity()
		e.archetype = this
		e
	}

	if (!isSentinel) {
		GameArchetype.addArchetype(this)
	}
}

object GameArchetype extends TArchetypeKind{
	val Sentinel: GameArchetype = new GameArchetype("Sentinel", GameArchetype) with TSentinel {}

	protected val _archetypes = new mutable.HashMap[String, Map[String, GameArchetype]]
	def archetype(kind: TArchetypeKind, identifier: String) = _archetypes.get(kind.kindStr)
			.flatMap(m => m.get(identifier.toLowerCase))
			.getOrElse(kind.Sentinel)
	def archetypes(kind: TArchetypeKind) = _archetypes.getOrElse(kind.kindStr, Map())

	def addArchetype(arch: GameArchetype): Unit = {
		addArchetype(arch, arch.kind)
	}
	protected def addArchetype(arch: GameArchetype, kind : TArchetypeKind): Unit = {
		val existing = _archetypes.getOrElse(kind.kindStr, Map())
		_archetypes.put(kind.kindStr, existing + (arch.identifier.toLowerCase.dropWhile(_ != '-').drop(1) -> arch))
		for (p <- kind.parentKind) {
			addArchetype(arch, p)
		}
	}

	def loadAllArchetypes(baseConfLocation: String, leafField: String, constr: (String, ConfigValue) => GameArchetype): Unit = {
		val byPackage = ConfigAssistant.loadAllConfigsByPackage(baseConfLocation, leafField)

		byPackage.foreach{ case (name, obj) =>
			val newArch = constr(name.toString, obj)
//			addArchetype(newArch) // no longer necessary, implicitly added on construction
		}
	}
}
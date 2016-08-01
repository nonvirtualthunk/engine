package arx.engine.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 10:17 AM
 */

import arx.Prelude._
import arx.core.traits.TSentinel
import arx.engine.data.TGameEntityAuxData
import arx.engine.data.THasInternalAuxData
import arx.engine.world.World
import scalaxy.loops._

class GameEntity(var name : String = "") extends TGameEntity with THasInternalAuxData[TGameEntityAuxData] {
	var id = TGameEntity.IdCounter.getAndIncrement
	if (name.isEmpty) {
		name = "GameEntity(" + id + ")"
	}
	protected var _archetype : Option[GameArchetype] = None
	override def archetype: Option[GameArchetype] = _archetype
	override def archetype_= (arc : GameArchetype) { _archetype = Some(arc) }

	override def hashCode() = id.hashCode()
	override def equals(obj: scala.Any): Boolean = obj match {
		case ge : GameEntity => ge.id == this.id
		case _ => false
	}
	override def toString() : String = this.archetype match {
		case Some(arch) => "GameEntity(" + arch.name + ", " + id + ")";
		case None => "GameEntity(" + id + ")";
	}
}


object GameEntity {
	val Sentinel : GameEntity = new GameEntity with TSentinel {

	}
}
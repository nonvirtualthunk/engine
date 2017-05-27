package arx.engine.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 10:17 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.traits.TSentinel
import arx.engine.data.TCopyOnWriteAuxData
import arx.engine.data.TGameEntityAuxData
import arx.engine.data.THasAuxData
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
	override def toString() : String = this.archetype match {
		case Some(arch) => "GameEntity(" + arch.name + ", " + id + ")";
		case None => "GameEntity(" + id + ")";
	}
}


class CopyOnWriteGameEntity(baseEntity : TGameEntity) extends TGameEntity with TCopyOnWriteAuxData[TGameEntityAuxData] {
	override def name: String = baseEntity.name
	override def archetype: Option[GameArchetype] = baseEntity.archetype
	override def archetype_=(arc: GameArchetype): Unit = {
		Noto.error("Cannot modify archetype in a copy-on-write game entity yet")
	}

	override def id: Long = baseEntity.id

	override def base: THasAuxData[TGameEntityAuxData] = baseEntity

	override def toString: String = s"CoW($baseEntity)"
}


object GameEntity {
	val Sentinel : GameEntity = new GameEntity with TSentinel {
		override def auxData[T <: TGameEntityAuxData : Manifest]: T = {
			Noto.severeError("Adding aux data to a sentinel, this is not allowed")
			super.auxData
		}
	}

	def deepEquals(a : TGameEntity, b : TGameEntity) : Boolean = {
		if (a.id != b.id) {
			false
		} else {
			val ad = a.allAuxData
			val bd = b.allAuxData
			if (ad.size != bd.size) {
				false
			} else {
				ad.forall(a => bd.contains(a))
			}
		}
	}
}
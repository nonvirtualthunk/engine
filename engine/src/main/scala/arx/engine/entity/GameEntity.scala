package arx.engine.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 10:17 AM
 */

import arx.Prelude._
import arx.engine.data.TGameEntityAuxData
import arx.engine.data.THasInternalAuxData
import arx.engine.world.World
import scalaxy.loops._

class GameEntity(var name : String = "") extends TGameEntity with THasInternalAuxData[TGameEntityAuxData] {
	var id = TGameEntity.IdCounter.getAndIncrement
	if (name.isEmpty) {
		name = "GameEntity(" + id + ")"
	}

	override def hashCode() = id.hashCode()
	override def equals(obj: scala.Any): Boolean = obj match {
		case ge : GameEntity => ge.id == this.id
		case _ => false
	}
}

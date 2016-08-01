package arx.engine.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 8:16 AM
 */

import java.util.concurrent.atomic.AtomicLong

import arx.Prelude._
import arx.core.introspection.CopyAssistant
import arx.core.traits.TSentinelable
import arx.engine.data.TGameEntityAuxData
import arx.engine.data.THasAuxData
import arx.engine.data.WrappedWithData
import arx.engine.world.World
import scalaxy.loops._

trait TGameEntity extends THasAuxData[TGameEntityAuxData] with TSentinelable {
	var world : World = World.Sentinel

	def name : String
	def archetype : Option[GameArchetype]
	def archetype_= (arc : GameArchetype)

	override def withData[R <: TGameEntityAuxData : Manifest] : WrappedWithData[TGameEntityAuxData,R,TGameEntity] =
		new WrappedWithData[TGameEntityAuxData, R, TGameEntity](this)

	def copyDataFrom[R <: TGameEntityAuxData : Manifest](other : TGameEntity): Unit = {
		this.manualAddAuxData(CopyAssistant.copy(other.aux[R]))
	}

	def <<[R <: TGameEntityAuxData](data : R) = {
		this.manualAddAuxData(data)
	}
}

object TGameEntity {
	val IdCounter = new AtomicLong(1L)
}
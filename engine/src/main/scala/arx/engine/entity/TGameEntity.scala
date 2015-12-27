package arx.engine.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 8:16 AM
 */

import java.util.concurrent.atomic.AtomicLong

import arx.Prelude._
import arx.engine.data.TGameEntityAuxData
import arx.engine.data.THasAuxData
import arx.engine.data.WrappedWithData
import arx.engine.world.World
import scalaxy.loops._

trait TGameEntity extends THasAuxData[TGameEntityAuxData] {
	var world : World = World.Sentinel

	def name : String


	override def withData[R <: TGameEntityAuxData : Manifest] : WrappedWithData[TGameEntityAuxData,R,TGameEntity] =
		new WrappedWithData[TGameEntityAuxData, R, TGameEntity](this)
}

object TGameEntity {
	val IdCounter = new AtomicLong(1L)
}
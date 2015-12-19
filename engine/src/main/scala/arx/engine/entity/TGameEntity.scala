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
import arx.engine.world.World
import scalaxy.loops._

trait TGameEntity extends THasAuxData[TGameEntityAuxData] {
	var world : World = World.Sentinel
}

object TGameEntity {
	val IdCounter = new AtomicLong(1L)
}
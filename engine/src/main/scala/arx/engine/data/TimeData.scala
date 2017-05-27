package arx.engine.data

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 12/22/15
  * Time: 3:15 PM
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.units.UnitOfTime
import scalaxy.loops._
import arx.core.vec._

class TimeData extends TWorldAuxData {
	var time : UnitOfTime = zeroSeconds

	protected[engine] var _timeAcceleration = 1.0f
	protected[engine] var _timeWarp = Option.empty[Float]

	def timeAcceleration = _timeAcceleration
	def timeWarp = _timeWarp

	def activateTimeWarp(f : Float): Unit = {
		if (f < 1.0f) {
			Noto.info("Time warp of < 1 doesn't really make sense, use time acceleration instead")
		} else if (f =~= 1.0f) {
			_timeWarp = None
		} else {
			_timeWarp = Some(f)
			_timeAcceleration = 1.0f
		}
	}
	def deactivateTimeWarp(): Unit = {
		_timeWarp = None
	}
	def activateTimeAcceleration(f : Float): Unit = {
		if (f >= 0.0f) {
			_timeAcceleration = f
			_timeWarp = None
		} else {
			Noto.warn("Negative time acceleration is not permitted")
		}
	}
	def pause(): Unit = {
		_timeAcceleration = 0.0f
	}

	override def equals(obj: scala.Any): Boolean = obj match {
		case o : TimeData => time == o.time && timeAcceleration == o.timeAcceleration && timeWarp == o.timeWarp
		case _ => false
	}
}

package arx.core.units

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/18/15
 * Time: 6:49 PM
 */

import arx.Prelude._
import arx.core.Moddable
import arx.core.representation.ConfigValue
import scalaxy.loops._

class UnitOfMeasureAwareConfig (val conf : ConfigValue) extends AnyVal{
	def asDistance = UnitOfMeasure.parseUnitOfDistance(conf.str)
	def asGameTime = UnitOfMeasure.parseUnitOfGameTime(conf.str)
	def asWallTime = UnitOfMeasure.parse[UnitOfTime](conf.str)
	def asDensity = UnitOfMeasure.parseUnitOfDensity(conf.str)
	def asMass = UnitOfMeasure.parseUnitOfMass(conf.str)
	def asSpeed = UnitOfMeasure.parseRatio[UnitOfDistance,UnitOfTime,UnitOfSpeed](conf.str)
	def asAcceleration = UnitOfMeasure.parseRatio[UnitOfDistance,UnitOfTimeSquared,UnitOfAcceleration](conf.str)

	def distanceOrElse (d : Moddable[UnitOfDistance]) : Moddable[UnitOfDistance] =
		if (conf.isSentinel) { d } else { Moddable(this.asDistance) }
	def gameTimeOrElse (d : Moddable[UnitOfTime]) : Moddable[UnitOfTime] =
		if (conf.isSentinel) { d } else { Moddable(this.asGameTime) }
	def wallTimeOrElse (d : Moddable[UnitOfTime]) : Moddable[UnitOfTime] =
		if (conf.isSentinel) { d } else { Moddable(this.asWallTime) }
	def densityOrElse (d : Moddable[UnitOfDensity]) : Moddable[UnitOfDensity] =
		if (conf.isSentinel) { d } else { Moddable(this.asDensity) }
	def massOrElse (d : Moddable[UnitOfMass]) : Moddable[UnitOfMass] =
		if (conf.isSentinel) { d } else { Moddable(this.asMass) }
	def speedOrElse (d : Moddable[UnitOfSpeed]) : Moddable[UnitOfSpeed] =
		if (conf.isSentinel) { d } else { Moddable(this.asSpeed) }
	def accelerationOrElse (d : Moddable[UnitOfAcceleration]) : Moddable[UnitOfAcceleration] =
		if (conf.isSentinel) { d } else { Moddable(this.asAcceleration) }
}

package arx.core.units

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 9:34 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto

abstract class MeasurementUnit extends Serializable{
	val name : String
	val suffix : String
	var conversion : Float

	def lowerOrderOfMagnitude : Option[MeasurementUnit] = None
	def higherOrderOfMagnitude : Option[MeasurementUnit] = None

	override def toString = "MeasurementUnit(" + name + " suffix=\"" + suffix + "\" conversion = " + conversion + ")"

	def allHigherOrdersOfMagnitude : List[MeasurementUnit] = higherOrderOfMagnitude match { case Some ( m ) => m :: m.allHigherOrdersOfMagnitude ; case None => Nil }
	def allLowerOrdersOfMagnitude : List[MeasurementUnit] = lowerOrderOfMagnitude match { case Some(m) => m :: m.allLowerOrdersOfMagnitude ; case None => Nil }
	def allOrdersOfMagnitude = allLowerOrdersOfMagnitude.reverse ::: this :: allHigherOrdersOfMagnitude
}
object UniverseUnit extends MeasurementUnit {
	val name = "universe unit"
	val suffix = "u"
	var conversion = toScalar(metersPerUniverseUnit)
}
object Meter extends MeasurementUnit {
	val name = "meter"
	val suffix = "m"
	var conversion = toScalar(1.0)

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Decimeter)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Kilometer)
}
object Decimeter extends MeasurementUnit {
	val name = "decimeter"
	val suffix = "dm"
	var conversion = toScalar(0.1)

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Centimeter)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Meter)
}
object Centimeter extends MeasurementUnit {
	val name = "centimeter"
	val suffix = "cm"
	var conversion = toScalar(0.01)

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Millimeter)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Decimeter)
}
object Millimeter extends MeasurementUnit {
	val name = "millimeter"
	val suffix = "mm"
	var conversion = toScalar(0.001)

	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Centimeter)
}
object Kilometer extends MeasurementUnit {
	val name = "kilometer"
	val suffix = "km"
	var conversion = toScalar(1000.0)

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Meter)
}
object Kilogram extends MeasurementUnit {
	val name = "killogram"
	val suffix = "kg"
	var conversion = toScalar(1.0)

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Gram)
}
object Gram extends MeasurementUnit {
	val name = "gram"
	val suffix = "g"
	var conversion = toScalar(0.001)

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Milligram)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Kilogram)
}
object Milligram extends MeasurementUnit {
	val name = "milligram"
	val suffix = "mg"
	var conversion = toScalar(Gram.conversion * 0.001)

	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Gram)
}
object Kelvin extends MeasurementUnit {
	val name = "kelvin"
	val suffix = "k"
	var conversion = toScalar(1.0f)
}
object Voxel extends MeasurementUnit {
	val name = "voxel"
	val suffix = "v"
	var conversion = toScalar(metersPerVoxel)
	def updateConversion () { conversion = metersPerVoxel }

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Octel)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Meter)
}
object Octel extends MeasurementUnit {
	val name = "octel"
	val suffix = "o"
	var conversion = toScalar((1.0f / 8.0f) * Voxel.conversion)
}
object Second extends MeasurementUnit {
	val name = "second"
	val suffix = "s"
	var conversion = toScalar(1.0f)

	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Minute)
}
object Minute extends MeasurementUnit {
	val name = "minute"
	val suffix = "min"
	var conversion = toScalar(60.0f)

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Second)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Hour)
}
object Hour extends MeasurementUnit {
	val name = "hour"
	val suffix = "hour"
	var conversion = toScalar(Minute.conversion * 60.0)

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Minute)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Day)
}
object Day extends MeasurementUnit {
	val name = "day"
	val suffix = "day"
	var conversion = toScalar(Hour.conversion * 24.0)

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Hour)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Season)
}
object Year extends MeasurementUnit {
	val name = "year"
	val suffix = "year"
	var conversion = toScalar(Day.conversion * 365.0)

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Season)
}
object Season extends MeasurementUnit {
	val name = "season"
	val suffix = "season"
	var conversion = toScalar(Year.conversion / 4.0) //4 seasons per year

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Day)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Year)
}

/** 16 seconds real-time - corresponds to Minute*/
object Moment extends MeasurementUnit {
	val name = "moment"
	val suffix = "m"
	var conversion = toScalar(Second.conversion * 16.0)

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Second)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Watch)
}
/** 2 minutes real-time - corresponds to Hour*/
object Watch extends MeasurementUnit {
	val name = "watch"
	val suffix = "watch"
	var conversion = toScalar(Moment.conversion * 8.0)

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Moment)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Cycle)
}
/** 16 minutes real-time - corresponds to Day*/
object Cycle extends MeasurementUnit {
	val name = "cycle"
	val suffix = "cycle"
	var conversion = toScalar(Watch.conversion * 8.0)

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Watch)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Sowing)
}
/** ~ 1 hour real-time - corresponds to Season/Month*/
object Sowing extends MeasurementUnit {
	val name = "sowing"
	val suffix = "sowing"
	var conversion = toScalar(Cycle.conversion * 4.0)

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Cycle)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Turning)
}
/** ~ 4.5 hours real-time - corresponds to Year*/
object Turning extends MeasurementUnit {
	val name = "turning"
	val suffix = "turning"
	var conversion = toScalar(Sowing.conversion * 4.0)

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Sowing)
}
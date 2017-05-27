package arx.core.units

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 10:15 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.Prelude

class UnitOfMeasureFloat(val f : Float) extends AnyVal {
	def universe = new UnitOfDistance(UniverseUnit,f)
	def uu = universe
	def uus = universe
	def meters = new UnitOfDistance(Meter,f)
	def meter = this.meters
	def voxels = new UnitOfDistance(Voxel,f)
	def voxel = this.voxels
	def centimeters = new UnitOfDistance(Centimeter,f)
	def centimeter = this.centimeters
	def cm = this.centimeters
	def millimeters = new UnitOfDistance(Millimeter,f)
	def squareMeters = new UnitOfArea(Meter,f)
	def squareMeter = this.squareMeters
	def squareCentimeters = new UnitOfArea(Centimeter,f)
	def meters2 = this.squareMeters
	def voxel2 = new UnitOfArea(Voxel,f)
	def voxel3 = new UnitOfVolume(Voxel,f)
	def centimeters2 = squareCentimeters
	def cubicMeters = new UnitOfVolume(Meter,f)
	def cubicCentimeters = new UnitOfVolume(Centimeter,f)
	def mm = new UnitOfDistance(Millimeter,f)
	def meters3 = this.cubicMeters
	def meter3 = this.cubicMeters
	def m3 = this.meter3
	def centimeters3 = cubicCentimeters
	def feet = new UnitOfDistance(Meter,f * 0.3048f)
	def inches = new UnitOfDistance(Centimeter,f * 2.54f)
	def km = new UnitOfDistance(Kilometer,f)
	def kilometers = this.km
	def kilometer = this.km

	def seconds = new UnitOfTime(Second,f)
	def milliseconds = new UnitOfTime(Second,f / 1000.0f)
	def nanoseconds = new UnitOfTime(Second,f / 1000000000.0f)
	def second = this.seconds
	def minute = new UnitOfTime(Minute,f)
	def minutes = this.minute
	def hour = new UnitOfTime(Hour,f)
	def hours = this.hour
	def day = new UnitOfTime(Day,f)
	def days = this.day
	def season = new UnitOfTime(Season,f)
	def seasons = this.season
	def year = new UnitOfTime(Year,f)
	def years = this.year

	/** In-game equivalent of a minute , ~16 seconds clock time, 8 to a watch*/
	def moment = new UnitOfTime(Moment,f)
	def moments = moment
	/** In-game equivalent of an hour, ~2 minutes clock time, 8 to a cycle */
	def watch = new UnitOfTime(Watch,f)
	/** In-game equivalent of a day, ~16 minutes clock time, 4 to a sowing */
	def cycle = new UnitOfTime(Cycle,f)
	/** In-game equivalent of a month/season, ~1 hour clock time, 4 to a turning */
	def sowing = new UnitOfTime(Sowing,f)
	/** In-game equivalent of a year, ~4.5 hours clock time */
	def turning = new UnitOfTime(Turning,f)

	def second2 = new UnitOfTimeSquared(Second,f)

	def kg = new UnitOfMass(Kilogram,f)
	def kilograms = this.kg
	def kilogram = this.kg
	def pounds = new UnitOfMass(Kilogram,f * 0.45359f) //auto-convert to metric
	def grams = new UnitOfMass(Gram,f)
	def gram = this.grams
	def mg = new UnitOfMass(Milligram,f)
	def milligram = this.mg
	def milligrams = this.mg

	def kelvin = new UnitOfTemperature(f)
	def degreesKelvin = this.kelvin
	def degreesCelsius = new UnitOfTemperature(f + 273.15f)
	def degreesCentigrade = this.degreesCelsius
	def degreesFahrenheit = new UnitOfTemperature((f + 459.67f)*0.5555555f)
	def centigrade = this.degreesCentigrade

	def m_s = new UnitOfSpeed( new UnitOfDistance(Meter,f) , Prelude.second )
	def v_s = new UnitOfSpeed( new UnitOfDistance(Voxel,f) , Prelude.second )
	def m_s2 = new UnitOfAcceleration( new UnitOfDistance(Meter,f) , Prelude.second2 )
	def km_h = new UnitOfSpeed( 1.meter , 1.hour )

	def kg_m3 = new RatioUnitOfMeasure[UnitOfMass,UnitOfVolume]( new UnitOfMass(Kilogram,f) , Prelude.m3 )
	def g_cm3 = new UnitOfDensity(new UnitOfMass(Gram,f) , Prelude.cm3)

	def x ( o : Float ) : UnitlessDimensions2 = new UnitlessDimensions2(f,o)
}
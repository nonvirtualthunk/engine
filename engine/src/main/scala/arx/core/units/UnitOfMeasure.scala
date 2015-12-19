package arx.core.units

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 9:35 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._


import arx.application.Noto
import arx.core.introspection.ReflectionAssistant
import java.io.{ObjectInput, ObjectOutput}
import arx.Prelude

abstract class UnitOfMeasure[T <: UnitOfMeasure[T]] ( var unit : MeasurementUnit , var value : Float ) extends Serializable {
//		def this() { this(null,0.0f) }
	def + ( u : T ) = if ( u.unit eq unit ) { create(unit,value + u.value) } else { create(baseUnitOfMeasure,this.toBaseUnitOfMeasure + u.toBaseUnitOfMeasure) }
	def - ( u : T ) = if ( u.unit eq unit ) { create(unit,value - u.value) } else { create(baseUnitOfMeasure,this.toBaseUnitOfMeasure - u.toBaseUnitOfMeasure) }
	def unary_- : T = create(unit,-value)
	def * ( scalar : Float ) = create(unit,value * scalar)
	def / ( scalar : Float ) = create(unit,value / scalar)
	def /[U <: UnitOfMeasure[U]] ( o : U ) : RatioUnitOfMeasure[T,U] = new RatioUnitOfMeasure[T,U](create(unit,value),o)
	def per[U <: UnitOfMeasure[U]] ( o : U ) : RatioUnitOfMeasure[T,U] = new RatioUnitOfMeasure[T,U](create(unit,value),o)
	def div[O <: UnitOfMeasure[O]] ( r : RatioUnitOfMeasure[T,O] ) = {
		val overBase = r.overValue.toBaseUnitOfMeasure
		r.underValue.create(r.underValue.baseUnitOfMeasure,r.underValue.toBaseUnitOfMeasure * (toBaseUnitOfMeasure / (if(overBase==0.0f){0.000001f}else{overBase})))
	}
	def < ( u : T ) = if ( u.unit eq this.unit ) { this.value < u.value } else { this.toBaseUnitOfMeasure < u.toBaseUnitOfMeasure }
	def == ( u : T ) = if ( ! (u eq null) ) { comparate(u,(a,b) => scala.math.abs(a - b) < 0.0001f) } else { false }
	def != ( u : T ) = !(this == u)
	def <= ( u : T ) = if ( u.unit eq this.unit ) { this.value <= u.value } else { this.toBaseUnitOfMeasure <= u.toBaseUnitOfMeasure }
	def > ( u : T ) = if ( u.unit eq this.unit ) { this.value > u.value } else { this.toBaseUnitOfMeasure > u.toBaseUnitOfMeasure }
	def >= ( u : T ) = if ( u.unit eq this.unit ) { this.value >= u.value } else { this.toBaseUnitOfMeasure >= u.toBaseUnitOfMeasure }

	override def equals ( other : Any ) : Boolean = {
		other match {
			case u : UnitOfMeasure[T] => u.order == this.order && u.baseUnitOfMeasure == this.baseUnitOfMeasure && scala.math.abs(u.toBaseUnitOfMeasure - this.toBaseUnitOfMeasure) < 0.0001f
			case _ => false
		}
	}
	override def toString = value + unit.suffix + (if ( order > 1 ) { "^" + order } else { "" })
	def displayString = toLongString
	def toLongString = {
		val orderString = if (order <= 1) { "" } else if ( order == 2 ) { " squared" } else if ( order == 3 ) { " cubed" } else { " fourthed?" }
		if ( value =~= value.round ) {
			value.toInt + " " + unit.name + "s" + orderString
		} else {
			decimalFormatter.format(value) + " " + unit.name + "s" + orderString
		}
	}
	def shortDisplayString = {
		val orderString = if (order <= 1) { "" } else { order.toString }
		if ( value =~= value.round ) {
			if ( value.round == 0 ) {
				unit.lowerOrderOfMagnitude match {
					case Some(lower) => {
						val inLower = this.toBaseUnitOfMeasure / math.pow(lower.conversion,order).toFloat
						if ( inLower =~= 0.0f ) {
							value.toInt + " " + unit.suffix + orderString
						} else {
							decimalFormatter.format(inLower) + " " + lower.suffix + orderString
						}
					}
					case None => value.toInt + " " + unit.suffix + orderString
				}
			} else { value.toInt + " " + unit.suffix + orderString }
		} else {
			val valueString = decimalFormatter.format(value)
			val baseOutput = valueString + " " + unit.suffix + orderString
			if ( valueString == "0" ) {
				unit.lowerOrderOfMagnitude match {
					case Some(lower) => {
						val inLower = this.toBaseUnitOfMeasure / math.pow(lower.conversion,order).toFloat
						if ( inLower =~= 0.0f ) {
							baseOutput
						} else {
							decimalFormatter.format(inLower) + " " + lower.suffix + orderString
						}
					}
					case None => baseOutput
				}
			} else {
				baseOutput
			}
		}
	}
	def toFloat = value

	def comparate ( u:T,f : (Float,Float) => Boolean ) : Boolean = { f(this.toBaseUnitOfMeasure,u.toBaseUnitOfMeasure) }
	def operate ( u : T , f : (Float,Float) => Float ) : T = {
		if ( u.unit == unit ) { create(unit,f(value,u.value)) }
		else { create(baseUnitOfMeasure,f(toBaseUnitOfMeasure,u.toBaseUnitOfMeasure)) }
	}
	def baseUnitOfMeasure : MeasurementUnit
	def toBaseUnitOfMeasure : Float
	def create ( unit : MeasurementUnit , value : Float ) : T
	def order : Int

	def higherOrder : Option[UnitOfMeasure[_]] = None
	def lowerOrder : Option[UnitOfMeasure[_]] = None

	def writeExternal(p1: ObjectOutput) {
		p1.writeFloat(value)
		p1.writeObject(unit)
	}
	def readExternal(p1: ObjectInput) {
		value = p1.readFloat()
		unit = p1.readObject().asInstanceOf[MeasurementUnit]
	}
}

//	class GenericUnitOfMeasure( unit_ : MeasurementUnit , value_ : Scalar ) extends UnitOfMeasure[GenericUnitOfMeasure](unit_, value_ ) {
//		def baseUnitOfMeasure = unit_.Lowest
//		def toBaseUnitOfMeasure = null
//		def create(unit: MeasurementUnit, value: Prelude.Scalar) = null
//		def order = 0
//	}

class RatioUnitOfMeasure[Over <: UnitOfMeasure[Over],Under <: UnitOfMeasure[Under]](overValueArg : Over,underValueArg: Under) extends Serializable {
	val underValue = if ( underValueArg.value == 0.0f ) {
		Noto.warn("Creating invalid under-value in ratio unit of measure");
		underValueArg.create(underValueArg.unit,1.0f)
	} else {
		underValueArg
	}
	val overValue = if ( underValueArg.value == 0.0f ) { overValueArg.create(overValueArg.unit,0.0f) } else { overValueArg }

	def * ( u : Under ) : Over = {
		val underBase = underValue.toBaseUnitOfMeasure
		val r = if ( underBase == 0.0f ) { 0.0f } else { u.toBaseUnitOfMeasure / underBase }
		overValue * r
	}

	def > ( r : RatioUnitOfMeasure[Over,Under] ) = this.toBaseUnitOfMeasure > r.toBaseUnitOfMeasure
	def < ( r : RatioUnitOfMeasure[Over,Under] ) = this.toBaseUnitOfMeasure < r.toBaseUnitOfMeasure
	def * ( f : Float ) : this.type = create(overValue * f,underValue)
	def - ( r : RatioUnitOfMeasure[Over,Under] ) : this.type = {
		val thisO = this.overValue.toBaseUnitOfMeasure
		val thisU = this.underValue.toBaseUnitOfMeasure

		val thatO = r.overValue.toBaseUnitOfMeasure
		val thatU = r.underValue.toBaseUnitOfMeasure

		create( overValue.create(overValue.baseUnitOfMeasure,(thisO/thisU) - (thatO/thatU)) , underValue.create(underValue.baseUnitOfMeasure,1.0f) )
//			create(this.overValue - r.overValue,this.underValue - r.underValue)
	}
	def + ( r : RatioUnitOfMeasure[Over,Under] ) : this.type = {
		val thisO = this.overValue.toBaseUnitOfMeasure
		val thisU = this.underValue.toBaseUnitOfMeasure

		val thatO = r.overValue.toBaseUnitOfMeasure
		val thatU = r.underValue.toBaseUnitOfMeasure

		create( overValue.create(overValue.baseUnitOfMeasure,(thisO/thisU) + (thatO/thatU)) , underValue.create(underValue.baseUnitOfMeasure,1.0f) )
//			create(this.overValue + r.overValue,this.underValue + r.underValue)
	}
	def unary_- = create(-this.overValue,this.underValue)
	def toBaseUnitOfMeasure = overValue.toBaseUnitOfMeasure / (if(underValue.toBaseUnitOfMeasure==0.0f){0.0000001f}else{underValue.toBaseUnitOfMeasure})
	override def equals ( other : Any ) : Boolean = {
		other match {
			case r : RatioUnitOfMeasure[Over,Under] =>
				val underBase = r.underValue.toBaseUnitOfMeasure
				val thisUnderBase = this.underValue.toBaseUnitOfMeasure
				r.overValue.baseUnitOfMeasure == this.overValue.baseUnitOfMeasure && r.overValue.order == this.overValue.order &&
				r.underValue.baseUnitOfMeasure == this.underValue.baseUnitOfMeasure && r.underValue.order == this.underValue.order &&
				scala.math.abs(	(if ( thisUnderBase == 0.0f ) { 0.0f } else { (this.overValue.toBaseUnitOfMeasure / thisUnderBase) }) -
								(if ( underBase == 0.0f ) { 0.0f } else { (r.overValue.toBaseUnitOfMeasure / (underBase)) })	) < 0.0001f
			case _ => false
		}
	}
	def create(o: Over, u: Under) : this.type = new RatioUnitOfMeasure(o,u).asInstanceOf[this.type]
	override def toString = {
		val normalized = if ( underValue == 0.0f ) { 0.0f } else { overValue.toFloat / underValue.toFloat}
		normalized + " " + overValue.unit.suffix + "/" + underValue.unit.suffix
	}

//		def writeExternal(p1: ObjectOutput) {p1.writeObject(overValue);p1.writeObject(underValue)}
//
//		def readExternal(p1: ObjectInput) {overValue = p1.readObject.asInstanceOf[Over];underValue = p1.readObject.asInstanceOf[Under]}
}
class UnitOfSpeed(d : UnitOfDistance,t : UnitOfTime) extends RatioUnitOfMeasure[UnitOfDistance,UnitOfTime](d,t) {
	def this() { this(0.0.meters,1.0.seconds) }
	def inMetersPerSecond = toBaseUnitOfMeasure
	def inVoxelsPerSecond = { val ts = t.inSeconds; if ( ts > 0.0001f ) { d.inVoxels / ts } else { 0.0f } }
	def inUniverseUnitsPerSecond = { val ts = t.inSeconds; if ( ts > 0.0001f ) { d.inUniverse / ts } else { 0.0f } }

	def / ( t2 : UnitOfTime ) : UnitOfAcceleration = { new UnitOfAcceleration(d,t * t2) }

	def getOrElse ( s : UnitOfSpeed ) : UnitOfSpeed = this
	def nonEmpty = true

	override def create(o: UnitOfDistance, u: UnitOfTime) : this.type = new UnitOfSpeed(o,u).asInstanceOf[this.type]

	def resolve() = this
	def baseValue() = this
}
object NoSpeed extends UnitOfSpeed(0.0f.meters,1.0.seconds) {
	override def getOrElse ( s : UnitOfSpeed ) = { s }
	override def nonEmpty = false
	override def toString = "NoSpeed"
}

class UnitOfDensity(m : UnitOfMass,v : UnitOfVolume) extends RatioUnitOfMeasure[UnitOfMass,UnitOfVolume](m,v) {
	def this() { this(0.0.kg,1.0.meter3) }
	def inKgPerMeter3 = toBaseUnitOfMeasure

	override def create(m : UnitOfMass,u : UnitOfVolume) : this.type = new UnitOfDensity(m,u).asInstanceOf[this.type]

	def resolve() = this
	def baseValue() = this
}

class UnitOfDistance(unit : MeasurementUnit,value: Float) extends UnitOfMeasure[UnitOfDistance](unit,value) {
	def abs = new UnitOfDistance(unit,math.abs(value))

	def this() { this(Meter,0.0f) }
	def inMeters : Float = value * unit.conversion
	def inUniverse : Float = (value * unit.conversion) / UniverseUnit.conversion
	def inCentimeters : Float = (value * unit.conversion) / Centimeter.conversion
	def inVoxels : Float = inMeters * voxelsPerMeter
	def * ( u : UnitOfDistance ) : UnitOfArea = {
		if ( unit == u.unit ) { new UnitOfArea(unit,value * u.value) }
		else { new UnitOfArea(Meter,inMeters * u.inMeters) }
	}
	def x ( u : UnitOfDistance ) : Dimensions2 = new Dimensions2(this,u)
	def copyWithValue( v : Float) = new UnitOfDistance(unit,v)

	def baseUnitOfMeasure = Meter
	def toBaseUnitOfMeasure = inMeters
	def create(u: MeasurementUnit, v: Float) = new UnitOfDistance(u,v)
	def order : Int = 1
	override def lowerOrder = Some(new UnitOfArea(unit,value))

	def resolve() = this
	def baseValue() = this
}
class UnitOfArea(unit : MeasurementUnit,value: Float) extends UnitOfMeasure[UnitOfArea](unit,value) {
	def this() { this(Meter,0.0f) }
	def inSquareMeters : Float = value * unit.conversion * unit.conversion
	def inSquareVoxels : Float = inSquareMeters * voxelsPerMeter * voxelsPerMeter
	def * ( u : UnitOfDistance ) : UnitOfVolume = {
		if ( unit == u.unit ) { new UnitOfVolume(unit,value * u.value) }
		else { new UnitOfVolume(Meter,inSquareMeters * u.inMeters) }
	}
	def copyWithValue( v : Float) = new UnitOfArea(unit,v)

	def baseUnitOfMeasure = Meter
	def toBaseUnitOfMeasure = inSquareMeters
	def create(u: MeasurementUnit, v: Float) = new UnitOfArea(u,v)
	def order : Int = 2
	override def lowerOrder = Some(new UnitOfDistance(unit,value))
	override def higherOrder = Some(new UnitOfVolume(unit,value))

	def resolve() = this
	def baseValue() = this
}
class UnitOfVolume(unit : MeasurementUnit,value: Float) extends UnitOfMeasure[UnitOfVolume](unit,value) {
	def this() { this(Meter,0.0f) }
	def inCubicMeters : Float = value * unit.conversion * unit.conversion * unit.conversion
	def inCubicVoxels : Float = inCubicMeters * voxelsPerMeter * voxelsPerMeter * voxelsPerMeter
	def copyWithValue( v : Float) = new UnitOfVolume(unit,v)

	def baseUnitOfMeasure = Meter
	def toBaseUnitOfMeasure = inCubicMeters
	def create(u: MeasurementUnit, v: Float) = new UnitOfVolume(u,v)
	def order : Int = 3
	override def lowerOrder = Some(new UnitOfArea(unit,value))

	def resolve() = this
	def baseValue() = this
}
class UnitOfMass(unit : MeasurementUnit,value: Float) extends UnitOfMeasure[UnitOfMass](unit,value) {
	def this () { this(Kilogram,0.0f) }
	def inKilograms : Float = value * unit.conversion
	def inKg = inKilograms

	def baseUnitOfMeasure = Kilogram
	def toBaseUnitOfMeasure = inKg
	def create(u: MeasurementUnit, v: Float) = new UnitOfMass(u,v)
	def order : Int = 1

	def resolve() = this
	def baseValue() = this
}
class UnitOfTime(unit : MeasurementUnit,value: Float) extends UnitOfMeasure[UnitOfTime](unit,value) {
	def this () { this(Second,0.0f) }
	def * (u : UnitOfTime) = new UnitOfTimeSquared(baseUnitOfMeasure,u.toBaseUnitOfMeasure * this.toBaseUnitOfMeasure)
	def inSeconds : Float = value * unit.conversion
	def inNanoseconds : Float = inSeconds * 1000000000.0f
	def inMilliseconds : Float = inSeconds * 1000.0f
	def inSowings : Float = inSeconds / Sowing.conversion
	def inTurnings : Float = inSeconds / Turning.conversion
	def inMinutes : Float = inSeconds / Minute.conversion
	def inWatches : Float = inSeconds / Watch.conversion
	def inCycles : Float = inSeconds / Cycle.conversion


	def baseUnitOfMeasure = Second
	def toBaseUnitOfMeasure = inSeconds
	def in(newUnit:MeasurementUnit) = this.inSeconds / newUnit.conversion
	def create(u: MeasurementUnit, v: Float) = new UnitOfTime(u,v)
	def order : Int = 1
	override def higherOrder = Some(new UnitOfTimeSquared(unit,value))

	def resolve() = this
	def baseValue() = this
}
class UnitOfTimeSquared(unit : MeasurementUnit,value: Float) extends UnitOfMeasure[UnitOfTimeSquared](unit,value) {
	def this () { this(Second,0.0f) }
	def inSecondsSquared = value * unit.conversion * unit.conversion
	def baseUnitOfMeasure = Second
	def toBaseUnitOfMeasure = inSecondsSquared
	def create(u: MeasurementUnit, v: Float) = new UnitOfTimeSquared(u,v)
	def order = 2
	override def lowerOrder = Some(new UnitOfTime(unit,value))

	def resolve() = this
	def baseValue() = this
}
class UnitOfAcceleration ( dist : UnitOfDistance , time : UnitOfTimeSquared ) extends RatioUnitOfMeasure[UnitOfDistance,UnitOfTimeSquared](dist,time) {
	def this() { this(new UnitOfDistance,new UnitOfTimeSquared(Second,1)) }
	def * ( t : UnitOfTime ) = new UnitOfSpeed(dist,new UnitOfTime(Second,underValue.inSecondsSquared / (if(t.inSeconds==0.0f){0.00000001f}else{t.inSeconds})))
	def inMetersPerSecondSquared = toBaseUnitOfMeasure
	def inVoxelsPerSecondSquared = { val ts = time.inSecondsSquared; if ( ts > 0.0001f ) { dist.inVoxels / ts } else { 0.0f } }

	override def create(o: UnitOfDistance, u: UnitOfTimeSquared) = new UnitOfAcceleration(o,u).asInstanceOf[this.type]

	def resolve() = this
	def baseValue() = this
}
class UnitOfTemperature(degrees:Float) extends UnitOfMeasure[UnitOfTemperature](Kelvin,degrees) {
	def this () { this(0.0f) }
	def inCentigrade = this.toBaseUnitOfMeasure - 273.0f
	def inKelvin = this.toBaseUnitOfMeasure
	def baseUnitOfMeasure = Kelvin
	def toBaseUnitOfMeasure = degrees * unit.conversion
	def create(unit: MeasurementUnit, v: Float) = new UnitOfTemperature(v)
	def order = 1
}

object UnitOfMeasure {
	def parseUnitOfMass(str: String): UnitOfMass = {
		val (alpha,numeric) = parseUnitOfMeasureParts(str)
		val f = numeric.toFloat
		alpha.toLowerCase match {
			case "tonnes" | "tonne" => (f * 1000).kg
			case "kg" => f.kg
			case "g" => (f * 0.001f).kg
			case _ => Noto.warn("Unknown mass unit in measure : " + alpha); (-1).kg
		}
	}

	def parseUnitOfDensity(str: String): UnitOfDensity = {
		if (str.endsWith("g/cm3") || str.endsWith("g/cm^3")) {
			val numeric = str.substring(0,str.indexOf("g"))
			numeric.toFloatOpt match {
				case Some (f) => f.g_cm3
				case None => {
					Noto.warn(s"Unparseable numeric part of density string '$str', could not parse '$numeric'")
					1.g_cm3
				}
			}
		} else {
			Noto.warn(f"Unrecognized density string '$str' could not parse unit postfix")
			1.g_cm3
		}
	}

	protected def parseUnitOfMeasureParts ( str : String ) : (String,String) = {
		val numeric = new StringBuilder
		val alpha = new StringBuilder
		var powerBit = false

		var index = 0
		while ( index < str.length ) {
			val char = str(index)
			if ( char.isLetter || char == '/' || char == '^' || powerBit ) {
				if ( numeric.isEmpty ) {
					Noto.warn(s"Invalid unit of measure string: $str")
					return ("","")
				} else if ( char == '^' ) {
					powerBit = true
				}

				alpha.append(char.toLower)
			} else if ( char.isDigit || char == '.') {
				if ( alpha.nonEmpty ) {
					Noto.warn(s"Invalid unit of measure string: $str")
					return ("","")
				}
				numeric.append(char)
				powerBit = false
			}
			index += 1
		}
		(alpha.toString(),numeric.toString())
	}
	def parseUnitOfDistance ( str: String ) : UnitOfDistance = {
		val (alpha,numeric) = parseUnitOfMeasureParts(str)
		if ( alpha.nonEmpty && numeric.nonEmpty ) {
			try {
				val f = numeric.toFloat
				val unit = alpha.toLowerCase match {
					case "m" => Meter
					case "meter" => Meter
					case "meters" => Meter
					case "cm" => Centimeter
					case "centimeter" => Centimeter
					case "centimeters" => Centimeter
					case "v" => Voxel
					case "voxel" => Voxel
					case "voxels" => Voxel
					case "km" => Kilometer
					case "kilometer" => Kilometer
					case "kilometers" => Kilometer
					case _ => Noto.warn(s"Unknown unit of distance : $alpha"); return (-4).meters
				}
				new UnitOfDistance(unit,f)
			} catch {
				case t : Throwable => Noto.warn(s"Invalid unit of distance string : $str"); return (-3).meters
			}
		} else { Noto.warn(s"Invalid unit of measure string : $str"); (-5).meters }
	}
	def parseUnitOfGameTime ( str: String ) : UnitOfTime = {
		val (alpha,numeric) = parseUnitOfMeasureParts(str)
		if ( alpha.nonEmpty && numeric.nonEmpty ) {
			try {
				val f = numeric.toFloat
				val unit = alpha.toLowerCase match {
					case "s" => Second
					case "second" => Second
					case "seconds" => Second
					case "moment" => Moment
					case "moments" => Moment
					case "watch" => Watch
					case "watches" => Watch
					case "cycle" => Cycle
					case "cycles" => Cycle
					case "sowing" => Sowing
					case "sowings" => Sowing
					case "turning" => Turning
					case "turnings" => Turning
					//End literal game units, move to auto translation
					case "minute" => Moment
					case "minutes" => Moment
					case "hour" => Watch
					case "hours" => Watch
					case "day" => Cycle
					case "days" => Cycle
					case "season" => Sowing
					case "seasons" => Sowing
					case "year" => Turning
					case "years" => Turning
					case _ => Noto.warn(s"Unknown unit of distance : $alpha"); return (-4).seconds
				}
				new UnitOfTime(unit,f)
			} catch {
				case t : Throwable => Noto.warn(s"Invalid unit of distance string : $str"); return (-3).seconds
			}
		} else { Noto.warn(s"Invalid unit of measure string : $str"); (-5).seconds }
	}

	def split (str : String, on : Char) = str.split(on) match { case arr : Array[String] => arr(0) -> arr(1) }
	def splitOutOrder (str : String) = {
		if (str.contains('^')) {
			val (alpha,order) = split(str,'^')
			(alpha,order.toInt)
		} else {
			(str,1)
		}
	}
	def unitForOrder[U <: UnitOfMeasure[U]] (baseUnit : UnitOfMeasure[U], order : Int) = {
		var tmp : Option[UnitOfMeasure[_]] = Some(baseUnit)
		while (tmp.nonEmpty && tmp.get.order < order) {
			tmp = tmp.get.higherOrder
		}
		tmp
	}

	def parse[U <: UnitOfMeasure[U] : Manifest] (str : String) : U = {
		val baseUnit = ReflectionAssistant.instantiate(manifest[U])

		val (alpha,numeric) = parseUnitOfMeasureParts(str)
		if (alpha.nonEmpty && numeric.nonEmpty) {
			val (unitStr,order) = splitOutOrder(alpha)
			if (order == baseUnit.order) {
				val unit = baseUnit.baseUnitOfMeasure.allOrdersOfMagnitude.find(u => u.name == unitStr || u.suffix == unitStr)

				unit match {
					case Some(u) => return baseUnit.create(u,numeric.toFloat)
					case None =>
				}
			} else {
				Noto.warn(s"Invalid order for parsing unit of measure, unit was $baseUnit, order was ${baseUnit.order}, string was : $str")
			}
		}

		// if we haven't returned something successfully by this point, it was invalid
		Noto.warn(s"Invalid unit of measure string : $str")
		baseUnit.create(baseUnit.baseUnitOfMeasure,-5)
	}

	def parseRatioAlpha(alpha: String) = {
		if (alpha.contains("per")) {
			val both = alpha.split("per")
			(both(0).trim.toLowerCase,both(1).trim.toLowerCase)
		} else if (alpha.contains("/")) {
			val both = alpha.split("/")
			(both(0).trim.toLowerCase,both(1).trim.toLowerCase)
		} else {
			Noto.warn(s"Invalid ratio unit of measure : $alpha")
			("","")
		}
	}

	def parseRatio[Over <: UnitOfMeasure[Over] : Manifest, Under <: UnitOfMeasure[Under] : Manifest, U <: RatioUnitOfMeasure[Over,Under] : Manifest] (str : String) : U = {
		val overBaseUnit = ReflectionAssistant.instantiate(manifest[Over])
		val underBaseUnit = ReflectionAssistant.instantiate(manifest[Under])


		val (alpha,numeric) = parseUnitOfMeasureParts(str)
		if (alpha.nonEmpty && numeric.nonEmpty) {
			val (overUnitAlphaRaw,underUnitAlphaRaw) = parseRatioAlpha(alpha)
			if (overUnitAlphaRaw.nonEmpty && underUnitAlphaRaw.nonEmpty) {
				val (overUnitAlpha,overUnitOrder) = splitOutOrder(overUnitAlphaRaw)
				val (underUnitAlpha,underUnitOrder) = splitOutOrder(underUnitAlphaRaw)

				if (overUnitOrder == overBaseUnit.order && underUnitOrder == underBaseUnit.order) {
					val overUnitOpt = overBaseUnit.baseUnitOfMeasure.allOrdersOfMagnitude.find(u => u.name == overUnitAlpha || u.name + "s" == overUnitAlpha || u.suffix == overUnitAlpha)
					val underUnitOpt = underBaseUnit.baseUnitOfMeasure.allOrdersOfMagnitude.find(u => u.name == underUnitAlpha || u.name + "s" == underUnitAlpha || u.suffix == underUnitAlpha)

					if (overUnitOpt.nonEmpty && underUnitOpt.nonEmpty) {
						val overUnit = overUnitOpt.get
						val underUnit = underUnitOpt.get

						val baseInst = ReflectionAssistant.instantiate(manifest[U])
						return baseInst.create(overBaseUnit.create(overUnit,numeric.toFloat),underBaseUnit.create(underUnit,1))
					}
				} else { Noto.warn(s"Invalid order (as in power) provided to ratio unit of measure when trying to parse for : ${manifest[U].runtimeClass.getSimpleName}")}
			}
		}

		Noto.warn(s"Invalid ratio unit of measure string : $str")
		ReflectionAssistant.instantiate(manifest[U])
	}

}

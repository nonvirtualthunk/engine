package arx

import java.io._
import java.text.DecimalFormat

import arx.core.Moddable
import arx.core.introspection.ReflectionAssistant
import arx.core.function._
import arx.core.mathutil.Solver
import arx.core.metrics.Metrics.RichTimer
import arx.core.representation.ConfigValue
import arx.application.Noto
import arx.core.MutableModdable
import arx.core.vec._
import arx.core.richer._
import arx.core.units._
import arx.core.vec.coordinates.VoxelCoord
import arx.resource.ResourceManager
import arx.serialization.DecompressibleInputStream
import com.codahale.metrics.Timer
import com.esotericsoftware.kryo.Kryo
import org.lwjgl.glfw.GLFW

import scala.io.Source
import scala.language.implicitConversions
import scala.util.Random

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/5/15
 * Time: 8:54 AM
 */

object Prelude {

	// ========================== Enrichment functions ==========================================
	implicit def funcToRunnable(f : () => Unit): FunctionRunnable = new FunctionRunnable(f)

	implicit def toArxString(str: String): ArxString = new ArxString(str)
	//	implicit def toString (str: ArxString) : String = str.intern
	implicit def toArxList[T] ( l : List[T] ) : ArxList[T] = { new ArxList(l) }
	implicit def toArxVector[T] ( l : Vector[T] ) : ArxVector[T] = { new ArxVector(l) }
	implicit def toArxIterable[T] ( l : Iterable[T] ) : ArxIterable[T] = new ArxIterable[T](l)
	implicit def toArxSet[T] ( l : Set[T] ) : ArxSet[T] = { new ArxSet(l) }
	implicit def toArxParSet[T] ( l : Set[T] ) : ArxParSet[T] = { new ArxParSet(l) }
	implicit def toArxFile (f : File) : ArxFile = new ArxFile(f)
	implicit def toArxClass[T] (c : Class[T]) : ArxClass[T] = new ArxClass[T](c)
	implicit def toRichTimer (t : Timer) : RichTimer = new RichTimer(t)
	implicit def toRicherBoolean (b : Boolean): RicherBoolean = new RicherBoolean(b)
	implicit def toArxOption[T] (opt : Option[T]): ArxOption[T] = new ArxOption(opt)
	implicit def toArxAnyRef[T <: AnyRef] (ref : T) : ArxAnyRef[T] = new ArxAnyRef[T](ref)
	implicit def toArxAny[T <: Any] (ref : T) : ArxAny[T] = new ArxAny[T](ref)
	implicit def toArxTuple[A,B] (tup : (A,B)) : ArxTuple[A,B] = new ArxTuple[A,B](tup)

	implicit def toArxKryo (k : Kryo): ArxKryo = new ArxKryo(k)

	implicit def unboxModdable[T] (m : Moddable[T]) : T = m.resolve()

	def MM (t : Float) = new MutableModdable(t)
	// ========================== Collections functions =========================================
	def fillArray[T : Manifest] ( size : Int )( f : (Int) => T ) = {
		val ret = manifest[T].newArray(size)
		var i = 0; while ( i < size  ) {
			ret(i) = f(i)
			i += 1}
		ret
	}

	def fillArray[T : Manifest] ( sizeX : Int, sizeY : Int)( f : (Int, Int) => T ) : Array[Array[T]] = {
		val ret = manifest[T].wrap.newArray(sizeX)
		var i = 0; while ( i < sizeX  ) {
			val tmp = manifest[T].newArray(sizeY)
			ret(i) = tmp
			var j = 0; while (j < sizeY) {
				tmp(j) = f(i,j)
				j += 1
			}
			i += 1}
		ret
	}

	def fillList[T] ( size : Int ) ( f : (Int) => T ) = {
		var ret = List[T]()
		var i = 0; while ( i < size ) {
			ret ::= f(i)
			i += 1}
		ret.reverse
	}

	def fillVector[T] ( size : Int ) ( f : (Int) => T ) = {
		var ret = Vector[T]()
		var i = 0; while ( i < size ) {
			ret :+= f(i)
			i += 1}
		ret
	}

	implicit class CaseInsensitiveString ( base : String ) {
		val string = base.toLowerCase

		override def hashCode(): Int = string.hashCode()
		override def equals(p1: scala.Any): Boolean = p1 match {
			case str : String => str.toLowerCase == string
			case cis : CaseInsensitiveString => cis.string == string
			case _ => false
		}
		override def toString = base
	}


	def none[T] : Option[T] = None

	// ========================== Meta functions =================================================
	def memoize[T,U] ( f : (T) => U ) = new MemoizingFunction[T,U](f)
	def memoize[S,T,U] ( f : (S,T) => U ) = new MemoizingFunction2[S,T,U](f)
	def memoize[R,S,T,U] ( f : (R,S,T) => U ) = new MemoizingFunction3[R,S,T,U](f)
	def memoize[Q,R,S,T,U] ( f : (Q,R,S,T) => U ) = new MemoizingFunction4[Q,R,S,T,U](f)

	def cacheMemoize[T <: AnyRef,U <: AnyRef] ( f : (T) => U ) = new MemoizingCachedFunction1(f)

	/** Partially memoizes a function, keeping only the most recent result cached */
	def memoizeSingle[T,U] ( f : (T) => U ) = new SingleMemoizingFunction(f)
	def memoizeSingle[S,T,U] ( f : (S,T) => U ) = new SingleMemoizingFunction2(f)


	@inline def bitSet( store : Int , flag : Int ) = (store & flag) == flag
	@inline def isBitSet(store : Int, flag : Int) = bitSet(store,flag)

	val ea = this.getClass.desiredAssertionStatus()

	def posit(condition: => Boolean, message: => String) {
		if (ea) {
			if (!condition) {
				throw new AssertionError(message)
			}
		}
	}

	// ========================== Introspection functions ========================================
	def provideInstanceOf[T <: AnyRef : Manifest] = ReflectionAssistant.provideInstanceOf[T]
	def pio[T <: AnyRef : Manifest] = ReflectionAssistant.provideInstanceOf[T]
	def PIO[T <: AnyRef : Manifest] = ReflectionAssistant.provideInstanceOf[T]

	// ========================== Unit of measure functions ======================================
	class UnitOfDistanceOrdering extends Ordering[UnitOfDistance] {
		def compare(x: UnitOfDistance, y: UnitOfDistance) = {
			if ( x > y ) { 1 }
			else if ( x < y ) { -1 }
			else { 0 }
		}
	}

	class UnitOfTimeOrdering extends Ordering[UnitOfTime] {
		def compare(x: UnitOfTime, y: UnitOfTime) = {
			if ( x > y ) { 1 }
			else if ( x < y ) { -1 }
			else { 0 }
		}
	}

	implicit val uom = new UnitOfDistanceOrdering
	implicit val utm = new UnitOfTimeOrdering

	def metersToVoxels ( f : Float ) = f * voxelsPerMeter
	def voxelsToMeters ( f : Float ) = f * metersPerVoxel

	var _voxelsPerMeter = 3f
	var _metersPerVoxel = 1.0f / _voxelsPerMeter
	def voxelsPerMeter_= ( v : Float ) { _voxelsPerMeter = v; _metersPerVoxel = 1.0f / v; Voxel.updateConversion() }
	def metersPerVoxel_= ( v : Float ) { _metersPerVoxel = v; _voxelsPerMeter = 1.0f / v; Voxel.updateConversion() }
	def voxelsPerMeter = _voxelsPerMeter
	def metersPerVoxel = _metersPerVoxel
	var metersPerUniverseUnit = 10.0f

	def toScalar ( d : Double ) : Float = d.toFloat
	def toScalar ( f : Float ) : Float = f


	val decimalFormatter = new DecimalFormat("#.##")

	def max[T <: UnitOfMeasure[T]] (a : T, b : T) = if (a > b) { a } else { b }
	def min[T <: UnitOfMeasure[T]] (a : T, b : T) = if (a < b) { a } else { b }

	implicit def rtodense ( r : RatioUnitOfMeasure[UnitOfMass,UnitOfVolume] ): UnitOfDensity =
		new UnitOfDensity(r.overValue,r.underValue)
	implicit def toUnitOfAcceleration( r : RatioUnitOfMeasure[UnitOfDistance,UnitOfTimeSquared] ): UnitOfAcceleration =
		new UnitOfAcceleration(r.overValue,r.underValue)
	implicit def rtoVelocity ( rum : RatioUnitOfMeasure[UnitOfDistance,UnitOfTime] ): UnitOfSpeed =
		new UnitOfSpeed(rum.overValue,rum.underValue)
	implicit def confToUoMParseable ( conf : ConfigValue ) : UnitOfMeasureAwareConfig =
		new UnitOfMeasureAwareConfig(conf)


	implicit def toUOMFloat (f : Float) : UnitOfMeasureFloat = new UnitOfMeasureFloat(f)
	implicit def toUOMFloat (d : Double) : UnitOfMeasureFloat = new UnitOfMeasureFloat(d.toFloat)
	implicit def toUOMFloat (i : Int) : UnitOfMeasureFloat = new UnitOfMeasureFloat(i)
	implicit def toUOMFloat (i : Short) : UnitOfMeasureFloat = new UnitOfMeasureFloat(i.toFloat)
	implicit def toUOMFloat (i : Long) : UnitOfMeasureFloat = new UnitOfMeasureFloat(i.toFloat)
	implicit def toUnitlessDimensions ( v : Vec3f ) : UnitlessDimensions3 = new UnitlessDimensions3(v.x,v.y,v.z)

	val meter3 = 1.meter3
	val m3 = meter3
	val cm3 = new UnitOfVolume(Centimeter,1.0f)
	val kg = 1.kg
	val meter = 1.meter
	val centimeter = 1.centimeter
	val meter2 = 1.meters2
	val cubicMeter = meter3
	val squareMeter = meter2
	val kg_m3 : UnitOfDensity = kg per meter3
	val meters = 1.meter
	val second = 1.second
	val second2 = 1.second2
	val year = 1.year
	val season = 1.season
	val oneMeterCube = 1.meter x 1.meter x 1.meter
	val zeroMeterCube = 0.meter x 0.meter x 0.meter
	val zeroSeconds = 0.seconds
	val zeroMetersPerSecond = 0.m_s
	val zeroMetersPerSecondSquared = 0.m_s2
	val zero_ms2 = 0.m_s2
	val zero_ms = 0.m_s
	val negativeMetersPerSecondSquared = (-1).m_s2
	val negaDimension = (-1).meter x (-1).meter x (-1).meter
	val oneVoxelCube = 1.voxel x 1.voxel x 1.voxel
	val quarterVoxelCube = 0.25.voxel x 0.25.voxel x 0.25.voxel
	val halfVoxelCube = 0.5.voxels x 0.5.voxels x 0.5.voxels
	val foreverTime = 10000000.years

	implicit var zeroMeters : UnitOfDistance = 0.meters
	implicit var zeroMeters2 : UnitOfArea = 0.meters2
	implicit var zeroMeters3 : UnitOfVolume = 0.meters3
	implicit var zeroKg : UnitOfMass = 0.kg


	def VC (x:Int,y:Int,z:Int) = VoxelCoord(x,y,z)
	def VC (xy:ReadVec2i,z:Int) = VoxelCoord(xy.x,xy.y,z)
	def VCR(dx:Int,dy:Int,dz:Int) = VoxelCoord.fromRelative(dx,dy,dz)

	// =========================== Math functions =================================================
	implicit def tup2FloatRange ( tup : (Float,Float) ) : FloatRange = new FloatRange(tup._1,tup._2)
	implicit def tup2IntRange ( tup : (Int,Int) ) : IntRange = new IntRange(tup._1,tup._2)
	implicit def single2IntRange ( sing : Int ) : IntRange = new IntRange(sing,sing)
	implicit def tup2MeasureRange[T <: UnitOfMeasure[T]] ( tup : (T,T) ) : UnitOfMeasureRange[T] = new UnitOfMeasureRange(tup._1,tup._2)

	implicit def float2RicherFloat ( f : Float ) : RicherFloat = new RicherFloat(f)
	implicit def double2RicherFloat ( f : Double ) : RicherDouble = new RicherDouble(f)
	implicit def tuple2EitherFloat ( t : (Float,Float) ) : EitherFloat = new EitherFloat(t._1,t._2)
	implicit def int2RicherInt ( i : Int ) : RicherInt = new RicherInt(i)

	implicit def toRicherTraversable[T] (traversable : Traversable[T]) : RicherTraversable[T] = new RicherTraversable[T](traversable)

	def euclidDistance ( x : Int , y : Int , v : ReadVec2i ) : Float = (x - v.x) * (x - v.x) + (y - v.y) * (y - v.y)
	def euclidDistance ( v1 : ReadVec2i , v2 : ReadVec2i ) : Float = (v1.x - v2.x) * (v1.x - v2.x) + (v1.y - v2.y) * (v1.y - v2.y)
	def euclidDistance ( v1 : ReadVec3f , v2 : ReadVec3f ) : Float = (v1.x - v2.x) * (v1.x - v2.x) + (v1.y - v2.y) * (v1.y - v2.y) + (v1.z - v2.z) * (v1.z - v2.z)
	def euclidDistance ( x : Int , y : Int , z : Int , v : Vec3i ) : Float = (x - v.x) * (x - v.x) + (y - v.y) * (y - v.y) + (z - v.z) * (z - v.z)
	def euclidDistance ( v1 : ReadVec3i , v2 : ReadVec3i ) : Float = (v1.x - v2.x) * (v1.x - v2.x) + (v1.y - v2.y) * (v1.y - v2.y) + (v1.z - v2.z) * (v1.z - v2.z)

	def manhattanDistance ( x : Int , y : Int , z : Int , v : Vec3i ) : Float = absf(x - v.x) + absf(y - v.y) + absf(z - v.z)

	def distance ( x : Int , y : Int , v : ReadVec2i ) : Float = {
		val euclidean = (x - v.x) * (x - v.x) + (y - v.y) * (y - v.y)
		if ( euclidean != 0 ) { sqrtf(euclidean) } else { 0.0f }
	}
	def distance ( v1 : ReadVec2i , v2 : ReadVec2i ) : Float = {
		val euclidean = (v1.x - v2.x) * (v1.x - v2.x) + (v1.y - v2.y) * (v1.y - v2.y)
		if ( euclidean != 0 ) { scala.math.sqrt(euclidean).toFloat } else { 0.0f }
	}
	def distance ( v1 : ReadVec3i , v2 : ReadVec3i ) : Float = {
		val euclidean = (v1.x - v2.x) * (v1.x - v2.x) + (v1.y - v2.y) * (v1.y - v2.y) + (v1.z - v2.z) * (v1.z - v2.z)
		if ( euclidean != 0 ) { scala.math.sqrt(euclidean).toFloat } else { 0.0f }
	}
	def distance ( x : Int , y : Int , z : Int , v : ReadVec3i ) : Float = {
		val euclidean = (x - v.x) * (x - v.x) + (y - v.y) * (y - v.y) + (z - v.z) * (z - v.z)
		if ( euclidean != 0 ) { scala.math.sqrt(euclidean).toFloat } else { 0.0f }
	}
	def distance ( v1 : Vec3f , v2 : ReadVec3f ) : Float = {
		val euclidean = (v1.x - v2.x) * (v1.x - v2.x) + (v1.y - v2.y) * (v1.y - v2.y) + (v1.z - v2.z) * (v1.z - v2.z)
		if ( euclidean != 0 ) { scala.math.sqrt(euclidean).toFloat } else { 0.0f }
	}
	def distance ( x : Int , y : Int , x2 : Int, y2: Int) : Float = {
		val euclidean = (x - x2) * (x - x2) + (y - y2) * (y - y2)
		if ( euclidean != 0 ) { sqrtf(euclidean) } else { 0.0f }
	}
	def distance ( v1 : ReadVec2f , v2 : ReadVec2f ) : Float = {
		val euclidean = (v1.x - v2.x) * (v1.x - v2.x) + (v1.y - v2.y) * (v1.y - v2.y)
		if ( euclidean != 0 ) { sqrtf(euclidean) } else { 0.0f }
	}

	implicit def floatList2RicherFloatList[T] ( l : List[T] ) : RicherFloatList[T] = new RicherFloatList(l)

	def rand ( low : Int , high : Int ) = low + (random.nextFloat() * (high - low)).toInt
	def rand ( low : Float , high : Float ) = low + (random.nextFloat() * (high - low))
	def rand ( r : Random , low : Float , high : Float ) = low + (r.nextFloat() * (high - low))
	def rand ( r : Random , low : Int , high : Int ) = low + (r.nextFloat() * (high - low)).toInt
	def randFrom[T] ( t : Seq[T] ) : T = t(rand(0,t.size))
	def randFrom[T] ( t : Set[T] ) : T = t.toSeq(rand(0,t.size))
	def randFrom[T] ( t : collection.mutable.Set[T] ) : T = t.toSeq(rand(0,t.size))
	def randFrom[T] ( r : Random , t : Seq[T] ) : T = t(rand(r,0,t.size))
	def randVec2 ( length : Float ) = {
		val theta = rand(0.0f,pi*2.0f)
		Vec2f( cosf(theta) * length, sinf(theta) * length )
	}


	def mix ( a : Float, b : Float , pcnt : Float ) = a + (b - a) * pcnt
	def mix ( a : Vec4f, b : Vec4f , pcnt : Float ) = a + (b - a) * pcnt
	def mix ( a : ReadVec3f, b : ReadVec3f , pcnt : Float ) = a + (b - a) * pcnt
	def mix(a: Int, b: Int, d: Double) = (a + (b - a) * d).toInt
	def mix(a: Int, b: Int, f: Float) = (a + (b - a) * f).toInt

	def min ( a : Vec3f , b: Vec3f ) = Vec3f(scala.math.min(a.x,b.x),scala.math.min(a.y,b.y),scala.math.min(a.z,b.z))
	def max ( a : Vec3f , b: Vec3f ) = Vec3f(scala.math.max(a.x,b.x),scala.math.max(a.y,b.y),scala.math.max(a.z,b.z))
	def min ( a : Vec3i , b: Vec3i ) = Vec3i(scala.math.min(a.x,b.x),scala.math.min(a.y,b.y),scala.math.min(a.z,b.z))
	def max ( a : Vec3i , b: Vec3i ) = Vec3i(scala.math.max(a.x,b.x),scala.math.max(a.y,b.y),scala.math.max(a.z,b.z))

	def roundf ( f : Float ) = math.round(f).toFloat

	@inline def sign ( i : Int ) = Integer.signum(i)
	@inline def sign ( f : Float ) = Math.signum(f)
	@inline def signN0 ( f : Float ) = if ( f < 0 ) { -1 } else { 1 }
	@inline def sameSign (a : Float, b : Float) = Math.signum(a) == Math.signum(b)

	def fract ( f : Float ) = f - math.floor(f).toFloat
	def toRad ( deg : Float ) = math.toRadians(deg).toFloat

	def pickByAxis[T](x : => T, y : => T, axis : Int) : T = axis match {
		case 0 => x
		case 1 => y
	}
	def pickByAxis[T](x : => T, y : => T, z : => T, axis : Int) : T = axis match {
		case 0 => x
		case 1 => y
		case 2 => z
	}

	protected def interpolateCosF ( x : Float , controlPoints : Seq[(Float,_)] ) : (Int,Int,Float) = {
		controlPoints match {
			case Nil => Noto.warn("can't interpolate with no control points");(0,0,0.0f)
			case one :: Nil => (0,0,0.0f)
			case someList => {
				val cp = someList.sortBy( _._1 )
				val b = cp.indexWhere( _._1 >= x ) match { case -1 => cp.size-1 ; case i => i }
				val a = cp.lastIndexWhere( _._1 < x ) match { case -1 => 0 ; case i => i }
				val mu = (x - controlPoints(a)._1) / scala.math.max(0.0001f,controlPoints(b)._1 - controlPoints(a)._1)
				val mu2 = (1.0f - scala.math.cos(mu * scala.math.Pi).toFloat) * 0.5f
				(a,b,mu2)
			}
		}
	}
	protected def interpolateLinF ( x : Float , controlPoints : Seq[(Float,_)] ) : (Int,Int,Float) = {
		controlPoints match {
			case Nil => Noto.warn("can't interpolate with no control points");(0,0,0.0f)
			case one :: Nil => (0,0,0.0f)
			case someList => {
				val cp = someList.sortBy( _._1 )
				val b = cp.indexWhere( _._1 >= x ) match { case -1 => cp.size-1 ; case i => i }
				val a = cp.lastIndexWhere( _._1 < x ) match { case -1 => 0 ; case i => i }
				val mu = (x - controlPoints(a)._1) / scala.math.max(0.0001f,controlPoints(b)._1 - controlPoints(a)._1)
				(a,b,mu)
			}
		}
	}
	protected def interpolateStepF ( x : Float , controlPoints : Seq[(Float,_)] ) : Int = {
		controlPoints.lastIndexWhere( _._1 <= x ) match {
			case -1 => controlPoints.size - 1
			case i => i
		}//0.0f 0.5f 1.0f
	}

	def linInterpolate ( x : Float , controlPoints : Seq[(Float,Float)] ) : Float = {
		val (ai,bi,mu2) = interpolateLinF(x,controlPoints)
		controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
	}
	def linInterpolatei ( x : Float , controlPoints : Seq[(Float,Int)] ) : Float = {
		val (ai,bi,mu2) = interpolateLinF(x,controlPoints)
		controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
	}
	def linInterpolate ( x : UnitOfMeasure[_] , controlPoints : Seq[(UnitOfMeasure[_],Float)] ) : Float = {
		linInterpolate(x.toBaseUnitOfMeasure,controlPoints.map(tup => tup._1.toBaseUnitOfMeasure -> tup._2))
	}
	def linInterpolate ( x : RatioUnitOfMeasure[_,_] , controlPoints : Seq[(RatioUnitOfMeasure[_,_],Float)] ) : Float = {
		linInterpolate(x.toBaseUnitOfMeasure,controlPoints.map(tup => tup._1.toBaseUnitOfMeasure -> tup._2))
	}
	def linInterpolatev4 ( x : Float , controlPoints : Seq[(Float,Vec4f)] ) = {
		val (ai,bi,mu2) = interpolateLinF(x,controlPoints)
		controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
	}
	def linInterpolatev4i ( x : Float , controlPoints : Seq[(Float,Vec4i)] ) = {
		val (ai,bi,mu2) = interpolateLinF(x,controlPoints)
		val v = controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
		Vec4i(v.r.toInt,v.g.toInt,v.b.toInt,v.a.toInt)
	}
	def linInterpolatev3 ( x : Float , controlPoints : Seq[(Float,ReadVec3f)] ) = {
		val (ai,bi,mu2) = interpolateLinF(x,controlPoints)
		controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
	}

	def cosInterpolate ( x : Float , controlPoints : Seq[(Float,Float)] ) = {
		val (ai,bi,mu2) = interpolateCosF(x,controlPoints)
		controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
	}
	def cosInterpolatev4 ( x : Float , controlPoints : Seq[(Float,ReadVec4f)] ) : ReadVec4f = {
		val (ai,bi,mu2) = interpolateCosF(x,controlPoints)
		controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
	}
	def cosInterpolatev3 ( x : Float , controlPoints : Seq[(Float,ReadVec3f)] ) : ReadVec3f = {
		val (ai,bi,mu2) = interpolateCosF(x,controlPoints)
		controlPoints(ai)._2 * (1.0f - mu2) + controlPoints(bi)._2 * mu2
	}

	def stepInterpolate4i ( x : Float , controlPoints : Seq[(Float,ReadVec4i)] ) = {
		require(controlPoints.nonEmpty)
		val index = interpolateStepF(x,controlPoints)
		controlPoints(index)._2
	}
	def stepInterpolate[T] ( x : Float , controlPoints : Seq[(Float,T)] ) = {
		require(controlPoints.nonEmpty)
		val index = interpolateStepF(x,controlPoints)
		controlPoints(index)._2
	}

	var random = new Random(1337L)
	def cosf(theta:Float) = scala.math.cos(theta).toFloat
	def acosf(theta:Float) = scala.math.acos(theta).toFloat
	def sinf(theta:Float) = scala.math.sin(theta).toFloat
	def asinf(theta:Float) = scala.math.asin(theta).toFloat
	def tanf(theta:Float) = scala.math.tan(theta).toFloat
	def cosf(theta:Double) = scala.math.cos(theta).toFloat
	def sinf(theta:Double) = scala.math.sin(theta).toFloat
	def tanf(theta:Double) = scala.math.tan(theta).toFloat
	def atanf(theta:Float) = scala.math.atan(theta).toFloat
	def atan2f(y:Float,x:Float) = scala.math.atan2(y,x).toFloat
	def sqrtf(x:Float) = scala.math.sqrt(x).toFloat
	def lengthf(x:Float,y:Float) = sqrtf(x*x+y*y)
	def lengthi(x:Int,y:Int) = sqrtf(x*x+y*y)
	def absf( x : Float )  = scala.math.abs(x)
	def absi( x : Int ) : Int = scala.math.abs(x)
	def powf( x : Float , e : Float ) : Float = scala.math.pow(x,e).toFloat
	def floorf ( x : Float ) = scala.math.floor(x).toFloat
	def ceilf ( x : Float ) = scala.math.ceil(x).toFloat

	def clamp ( f : Float , low : Float , high : Float ) = scala.math.min(scala.math.max(f,low),high)
	def clamp ( i : Int , low : Int , high : Int ) = scala.math.min(scala.math.max(i,low),high)

	def Po2Above ( i : Int ) = {
		val base = (scala.math.log(i) / scala.math.log(2)).toInt
		if ( (1 << base) < i ) { base + 1 } else { base }
	}

	def isPo2 (i:Int) = (1 << Po2Above(i)) == i

	def lengthSafe ( v : Vec3i ) = {
		val e = v.x*v.x+v.y*v.y+v.z*v.z
		if ( e != 0 ) { scala.math.sqrt(e).toFloat }
		else { 0.0f }
	}

	def lengthSafe ( v : Vec2i ) = {
		val e = v.x*v.x+v.y*v.y
		if ( e != 0 ) { scala.math.sqrt(e).toFloat }
		else { 0.0f }
	}

	def lengthSafe ( v : Vec3f ) = {
		val e = v.x*v.x+v.y*v.y+v.z*v.z
		if ( e != 0.0f ) { scala.math.sqrt(e).toFloat }
		else { 0.0f }
	}

	def lengthSafe ( v : Vec2f ) = {
		val e = v.x*v.x+v.y*v.y
		if ( e != 0.0f ) { scala.math.sqrt(e).toFloat }
		else { 0.0f }
	}

	def fastSqrt(a : Double) = {
		scala.math.sqrt(a)
		//		val x : Long = java.lang.Double.doubleToLongBits(a) >> 32;
		//		val y : Double = java.lang.Double.longBitsToDouble((x + 1072632448) << 31);
		//
		//		// repeat the following line for more precision
		//		//y = (y + a / y) * 0.5;
		//		y

	}

	def fastSqrt ( f : Float ) = { val x = java.lang.Float.floatToIntBits(f); val i = (1<<29) + (x >> 1) - (1<<22); java.lang.Float.intBitsToFloat(i) }

	def solveQuadratic ( a : Float , b: Float , c: Float ) = Solver.solveQuadratic(a,b,c)

	def toUnsignedInt ( b : Byte ) : Int = b & 0xff

	val pi = math.Pi.toFloat
	val π = pi


	// ================================== IO Functions ===================================
	def writeTextToFile ( file : File, str : String ) { writeTextToFile(file.getAbsolutePath,str) }
	def writeTextToFile ( path : String , str : String ) {
		val fw = new FileWriter(path)
		fw.write(str)
		fw.close()
	}
	def readTextFromFile ( path : String ) : String = readTextFromFile(new File(path))
	def readTextFromFile ( file : File ) : String = {
		if (!file.exists) {
			Noto.error(s"Requested file does not exist, cannot read text : ${file.getAbsolutePath}")
			""
		} else {
			val stream = new FileInputStream(file)
			val ret = readTextFromStream(stream)
			stream.close()
			ret
		}
	}
	def readTextFromResource (path : String) = {
		val stream = ResourceManager.getResourceStream(path)
		val ret = readTextFromStream(stream)
		stream.close()
		ret
	}
	def readTextFromStream(stream : InputStream) = {
		val source = Source.fromInputStream(stream)
		val ret = new StringBuilder
		for (lin <- source.getLines()) {
			ret.append(lin).append("\n")
		}
		source.close()
		ret.toString ()
	}
	def writeToFile ( path : String , o : Any, compress : Boolean ) {
		writeToFile(new File(path),o,compress)
	}
	def writeToFile ( path : String , o : Any) {
		writeToFile(path,o,compress = false);
	}
	def writeToFile ( file : File , o : Any ) {
		writeToFile(file,o,compress = false)
	}
	def writeToFile ( file : File , o : Any , compress : Boolean ) {
		val tmpFile = File.createTempFile("genericwrite","tmp")
		val fileOutputStream = new FileOutputStream(tmpFile)
		o match {
			case ba : Array[Byte] => fileOutputStream.write(ba)
			case _ => {
				val objectOutputStream = new ObjectOutputStream(fileOutputStream)
				objectOutputStream.writeObject(o)
				objectOutputStream.close()
			}
		}

		fileOutputStream.flush()
		fileOutputStream.close()
		tmpFile.renameTo(file)
	}

	def readFromFile[T] ( file : File ) : T = {
		val fileStream = new FileInputStream(file)
		val objectStream = new DecompressibleInputStream(fileStream)
		val ret = objectStream.readObject.asInstanceOf[T]
		objectStream.close()
		fileStream.close()
		ret
	}

	def listAllFiles ( file : File , recursive : Boolean ) : List[File] = {
		file :: (file.listFiles() match {
			case null => Nil
			case files => files.toList.flatMap( f => listAllFiles(f,recursive) )
		})
	}

	def writeToBytes ( obj : Any ) = {
		val bos = new ByteArrayOutputStream()
		val oos = new ObjectOutputStream(bos)
		oos.writeObject(obj)
		oos.close()
		bos.close()
		bos.toByteArray
	}

	def readFromBytes[T] ( bytes : Array[Byte] ) : T = {
		val bos = new ByteArrayInputStream(bytes)
		val oos = new ObjectInputStream(bos)
		val ret = oos.readObject()
		oos.close()
		bos.close()
		ret.asInstanceOf[T]
	}

	def curTimeSeconds() = GLFW.glfwGetTime().toFloat
	def curTimeMillis() = curTimeSeconds * 1000.0f
	def curTime() = GLFW.glfwGetTime().seconds

	implicit def toRicherNumeric[T : Numeric](v : T) = new RicherNumeric[T](v)
}

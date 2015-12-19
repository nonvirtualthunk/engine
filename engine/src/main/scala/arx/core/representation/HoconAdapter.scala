package arx.core.representation

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 4/15/14
 * Time: 7:20 AM
 */

import arx.Prelude._
import scalaxy.loops._
import com.typesafe.config.{ConfigValue => HoconConfigValue}
import com.typesafe.config.{ConfigList => HoconConfigList}
import com.typesafe.config._
import arx.core.Moddable
import arx.core.vec.{Vec4f, ReadVec2f, ReadVec3f, ReadVec4f}
import arx.core.function.{ArithmeticExpression, TArithmeticExpression}
import scala.collection.convert.wrapAsScala._
import scala.language.dynamics
import arx.application.Noto
import arx.core.traits.{TSentinel, TSentinelable}
import java.io.{Reader, File}
import com.typesafe.config.ConfigException.NotResolved

trait THasConfigParent {
	def parent : ConfigValue
}

trait ConfigList extends TSentinelable with Traversable[ConfigValue] with THasConfigParent {
	def apply (i:Int) : ConfigValue
}
trait ConfigValue extends TSentinelable with Dynamic with THasConfigParent {
	def selectDynamic(methodName: String) : ConfigValue

	def isEmpty = true
	def nonEmpty = ! isEmpty
	final def nonEmptyValue = ! isEmpty
	def hasField(s : String) : Boolean
	def field(s : String) : ConfigValue
	def fields : Map[String,ConfigValue]

	def orElse (other : ConfigValue) : ConfigValue = if (this.isSentinel) { other } else { this }
	def boolOrElse(b: Moddable[Boolean]): Moddable[Boolean]
	def v2OrElse(v: Moddable[ReadVec2f]): Moddable[ReadVec2f]
	def v3OrElse(v: Moddable[ReadVec3f]): Moddable[ReadVec3f]
	def v4OrElse(v: Moddable[ReadVec4f]): Moddable[ReadVec4f]
	def floatOrElse(f: Moddable[Float]): Moddable[Float]
	def strOrElse(s: Moddable[String]): Moddable[String]
	def intOrElse(i: Moddable[Int]): Moddable[Int]
	def boolOrElse(orElse: Boolean): Boolean
	def floatOrElse(f: Float): Float
	def strOrElse(s: String): String
	def intOrElse(i: Int): Int
	def v2OrElse(v: ReadVec2f): ReadVec2f
	def v3OrElse(v: ReadVec3f): ReadVec3f
	def v4OrElse(v: ReadVec4f): ReadVec4f
	def expressionOrElse(f:Float) : TArithmeticExpression
	def expression : TArithmeticExpression

	def v2: ReadVec2f
	def v3: ReadVec3f
	def v4: ReadVec4f
	def arr: ConfigList
	def isObj: Boolean
	def isStr: Boolean
	def isArr: Boolean
	def isRoot : Boolean = false
	def bool: Boolean
	def float: Float
	def int: Int
	def str: String
	
	def unwrapped : Any
	def rootConfigValue = (isRoot || isSentinel) match {
		case true => this
		case false => parent
	}
}
trait ConfigRoot extends ConfigValue {
	def uid : Int
}

class BasicConfigList (wrapped : Vector[ConfigValue], parentis : ConfigValue) extends ConfigList {
	override def foreach[U](f: (ConfigValue) => U): Unit = wrapped.foreach(f)
	override def size = wrapped.size
	override def isEmpty = wrapped.isEmpty
	override def apply(i: Int): ConfigValue = wrapped(i)
	override def parent: ConfigValue = parentis
}

object ConfigList {
	val Sentinel : ConfigList = new ConfigList with TSentinel {
		override def foreach[U](f: (ConfigValue) => U): Unit = {}
		override def size = 0
		override def isEmpty = true
		override def apply (i:Int) = ConfigValue.Sentinel
		override def parent: ConfigValue = ConfigValue.Sentinel
	}
}

object ConfigValue {
	/**
	 * Gets the requested field from the given value, looking upwards through the hierarchy
	 * until it finds a value containing the requested field, or a Sentinel if none are found
	 */
	def extractFieldRecursive(value : ConfigValue,fieldName : String) : ConfigValue = {
		if (value.hasField(fieldName)) {
			value.field(fieldName)
		} else if (value.parent.notSentinel) {
			extractFieldRecursive(value.parent,fieldName)
		} else {
			ConfigValue.Sentinel
		}
	}

	val Sentinel : ConfigValue = new ConfigValue with TSentinel {
		override def parent = this
		override def isEmpty = true
		override def isSentinel = true

		override def boolOrElse(b: Moddable[Boolean]): Moddable[Boolean] = b
		override def v2OrElse(v: Moddable[ReadVec2f]): Moddable[ReadVec2f] = v
		override def v3OrElse(v: Moddable[ReadVec3f]): Moddable[ReadVec3f] = v
		override def v4OrElse(v: Moddable[ReadVec4f]): Moddable[ReadVec4f] = v
		override def floatOrElse(f: Moddable[Float]): Moddable[Float] = f
		override def strOrElse(s: Moddable[String]): Moddable[String] = s
		override def intOrElse(i: Moddable[Int]): Moddable[Int] = i

		override def boolOrElse(orElse: Boolean): Boolean = orElse
		override def floatOrElse(f: Float): Float = f
		override def strOrElse(s: String): String = s
		override def intOrElse(i: Int): Int = i
		override def v2OrElse(v: ReadVec2f): ReadVec2f = v
		override def v3OrElse(v: ReadVec3f): ReadVec3f = v
		override def v4OrElse(v: ReadVec4f): ReadVec4f = v
		override def expressionOrElse(f:Float) = ArithmeticExpression.fromValue(f)
		override def expression = illegalAccess()

		override def v2: ReadVec2f = illegalAccess()
		override def v3: ReadVec3f = illegalAccess()
		override def v4: ReadVec4f = illegalAccess()
		override def arr: ConfigList = ConfigList.Sentinel
		override def isObj: Boolean = illegalAccess()
		override def isStr: Boolean = illegalAccess()
		override def isArr: Boolean = illegalAccess()
		override def bool: Boolean = illegalAccess()
		override def float: Float = illegalAccess()
		override def int: Int = illegalAccess()
		override def str: String = illegalAccess()
		
		protected def illegalAccess (): Nothing = {
			throw new IllegalStateException("Attempting to access empty value")
		}

		override def field(s: String): ConfigValue = illegalAccess()
		override def hasField(s: String): Boolean = false
		override def selectDynamic(methodName: String): ConfigValue = {
			illegalAccess()
		}
		override def fields : Map[String,ConfigValue] = Map()
		override def unwrapped = this
	}
}

object Hocon {
	def parse ( s : String ) = {
		val config = ConfigFactory.parseString(s).resolve()
		RichConfigRoot(config.root())
	}
	def parseResource ( s : String ) = {
		val config = ConfigFactory.parseResources(s).resolve()
		config.resolve()
		RichConfigRoot(config.root())
	}
	def parseFile ( f : File ) = {
		val config = ConfigFactory.parseFile(f).resolve()
		config.resolve()
		RichConfigRoot(config.root())
	}
	def parseReader ( r : Reader ) = {
		RichConfigRoot(parseReaderRaw(r).root())
	}
	def parseReaderRaw ( r : Reader ) = {
		val config = ConfigFactory.parseReader(r).resolve()
		config.resolve()
		config
	}
//	implicit class RichConfig ( val config : Config ) extends AnyVal with RichHoconBase {
//		def selectDynamic(methodName: String) = {
//			if (config.hasPath(methodName)) {
//				new RichConfigValue(config.getValue(methodName))
//			} else {
//				EmptyHoconValue
//			}
//		}
//
//		def getValue ( path : String ) = {
//			new RichConfigValue( config.getValue(path) )
//		}
//
//		def root = new RichConfigValue(config.root())
//	}
	object RichConfigValue {
		def apply ( value : HoconConfigValue , parent : ConfigValue ) : ConfigValue = {
			new RichConfigValue(value,parent)
		}
	}

	object RichConfigRoot {
		def apply ( value : HoconConfigValue ) : ConfigValue = {
			new RichConfigRoot(value)
		}
	}

	class RichConfigValue ( val value : HoconConfigValue , val parent : ConfigValue ) extends ConfigValue {
		override def isEmpty = false

		def selectDynamic(methodName: String) = {
			fields.getOrElse(methodName,ConfigValue.Sentinel)
		}

		def str = value.unwrapped().toString
		def int : Int = value.valueType() match {
			case ConfigValueType.STRING =>
				ArithmeticExpression.fromString(value.unwrapped().asInstanceOf[String]).evaluate().toInt
			case ConfigValueType.NUMBER => value.unwrapped() match {
				case i : java.lang.Integer => i
				case f : java.lang.Float => f.toInt
				case d : java.lang.Double => d.toInt
				case o => Noto.error("Invalid unwrapped type for call to int : " + o); -1
			}
			case _ => Noto.error(f"Unacceptable datatype (int requested, ${value.valueType()} found"); 0
		}
		def float : Float = value.valueType() match {
			case ConfigValueType.STRING =>
				ArithmeticExpression.fromString(value.unwrapped().asInstanceOf[String]).evaluate()
			case ConfigValueType.NUMBER => value.unwrapped() match {
				case i : java.lang.Integer => i.toFloat
				case f : java.lang.Float => f
				case d : java.lang.Double => d.toFloat
				case o => Noto.error("Invalid unwrapped type for call to float : " + o); 0.0f
			}
			case _ => Noto.error(f"Unacceptable datatype (float requested, ${value.valueType()} found"); 0.0f
		}
		def bool : Boolean = value.unwrapped() match {
			case s : String => s.toBooleanOpt match {
				case Some(b) => b
				case None => Noto.error(f"could not convert $s to boolean"); false
			}
			case b : java.lang.Boolean => b
			case o => Noto.error(f"Invalid unwrapped type for converting to boolean : $o"); false
		}
		def isArr = value.valueType() == ConfigValueType.LIST
		def isStr = value.valueType() == ConfigValueType.STRING
		def isObj = value.valueType() == ConfigValueType.OBJECT
		def arr = value match {
			case c : HoconConfigList => new RichHoconConfigList(c,this)
			case _ => new BasicConfigList(Vector(this),this)
		}
		def v4 = value match {
			case l : HoconConfigList => {
				val r = RichConfigValue(l.get(0),parent).float
				val g = RichConfigValue(l.get(1),parent).float
				val b = RichConfigValue(l.get(2),parent).float
				val a = RichConfigValue(l.get(3),parent).float
				ReadVec4f(r,g,b,a)
			}
		}
		def v3 = value match {
			case l : HoconConfigList => {
				val r = RichConfigValue(l.get(0),parent).float
				val g = RichConfigValue(l.get(1),parent).float
				val b = RichConfigValue(l.get(2),parent).float
				ReadVec3f(r,g,b)
			}
		}
		def v2 = value match {
			case l : HoconConfigList => {
				val r = RichConfigValue(l.get(0),parent).float
				val g = RichConfigValue(l.get(1),parent).float
				ReadVec2f(r,g)
			}
		}

		def expression = value.unwrapped() match {
			case s : String => ArithmeticExpression.fromString(s)
			case f : java.lang.Float => ArithmeticExpression.fromValue(f)
			case i : java.lang.Integer => ArithmeticExpression.fromValue(i)
			case o =>
				Noto.error(f"invalid type provided when attempting to convert config value to expression, was : $o")
				ArithmeticExpression.fromValue(0)
		}

		def v4OrElse ( v : ReadVec4f ) = v4
		def v3OrElse ( v : ReadVec3f ) = v3
		def v2OrElse ( v : ReadVec2f ) = v2
		def intOrElse ( i : Int ) = int
		def strOrElse ( s : String ) = str
		def floatOrElse ( f : Float ) = float
		def boolOrElse ( orElse : Boolean ) = bool
		def intOrElse ( i : Moddable[Int] ) : Moddable[Int] = Moddable(int)
		def strOrElse ( s : Moddable[String] ) : Moddable[String] = Moddable(str)
		def floatOrElse ( f : Moddable[Float] ) : Moddable[Float] = Moddable(float)
		def v4OrElse ( v :  	Moddable[ReadVec4f] ) : Moddable[ReadVec4f] = v4
		def v3OrElse ( v :  	Moddable[ReadVec3f] ) : Moddable[ReadVec3f] = v3
		def v2OrElse ( v : 	Moddable[ReadVec2f] ) : Moddable[ReadVec2f] = v2
		def boolOrElse ( b : Moddable[Boolean]   ) : Moddable[Boolean]   = Moddable(bool)

		def expressionOrElse ( f : Float ) : TArithmeticExpression = expression

		def hasField ( s : String ) = value match {
			case c : ConfigObject => c.containsKey(s)
			case _ => false
		}
		def field ( s : String ) = fields.getOrElse(s,ConfigValue.Sentinel)
		lazy val fields : Map[String,ConfigValue] = value match {
			case c : ConfigObject => c.entrySet().map( e => e.getKey -> RichConfigValue(e.getValue,this) ).toMap
			case o =>
				Noto.severeError("Attempting to get fields map from non-object config value")
				Map()
		}

		override def unwrapped: Any = {
			value match {
				case c : ConfigObject => c.unwrapped().toMap
				case l : HoconConfigList => l.unwrapped().toVector
				case v : HoconConfigValue => v.unwrapped() match {
					case s : String => s
					case f : java.lang.Float => f: Float
					case d : java.lang.Double => d.toFloat
					case i : java.lang.Integer => i : Int
					case b : java.lang.Boolean => b : Boolean
				}
			}
		}
	}

	protected var UIDCounter = 1
	class RichConfigRoot(valuer : HoconConfigValue) extends RichConfigValue(valuer,ConfigValue.Sentinel) with ConfigRoot{
		val uid = UIDCounter
		UIDCounter += 1

		override def isRoot : Boolean = true
	}

	class RichHoconConfigList ( val value : HoconConfigList, val parent : ConfigValue ) extends ConfigList with ConfigValue {
		lazy val convertedList = value.map(v => RichConfigValue(v,this))
		override def foreach[U](f: (ConfigValue) => U): Unit = {
			convertedList.foreach(f)
		}
		override def size = value.size
		override def isEmpty = value.isEmpty
		override def nonEmpty = ! isEmpty
		override def apply(i:Int) = convertedList(i)

		override def boolOrElse(b: Moddable[Boolean]): Moddable[Boolean] = b
		override def v2OrElse(v: Moddable[ReadVec2f]): Moddable[ReadVec2f] = v
		override def v3OrElse(v: Moddable[ReadVec3f]): Moddable[ReadVec3f] = v
		override def v4OrElse(v: Moddable[ReadVec4f]): Moddable[ReadVec4f] = v
		override def floatOrElse(f: Moddable[Float]): Moddable[Float] = f
		override def strOrElse(s: Moddable[String]): Moddable[String] = s
		override def intOrElse(i: Moddable[Int]): Moddable[Int] = i

		override def boolOrElse(orElse: Boolean): Boolean = orElse
		override def floatOrElse(f: Float): Float = f
		override def strOrElse(s: String): String = s
		override def intOrElse(i: Int): Int = i
		override def v2OrElse(v: ReadVec2f): ReadVec2f = v
		override def v3OrElse(v: ReadVec3f): ReadVec3f = v
		override def v4OrElse(v: ReadVec4f): ReadVec4f = v
		override def expressionOrElse(f:Float) = ArithmeticExpression.fromValue(f)
		override def expression = illegalAccess()

		override def v2: ReadVec2f = illegalAccess()
		override def v3: ReadVec3f = illegalAccess()
		override def v4: ReadVec4f = illegalAccess()
		override def arr: ConfigList = this
		override def isObj: Boolean = illegalAccess()
		override def isStr: Boolean = illegalAccess()
		override def isArr: Boolean = illegalAccess()
		override def bool: Boolean = illegalAccess()
		override def float: Float = illegalAccess()
		override def int: Int = illegalAccess()
		override def str: String = illegalAccess()

		protected def illegalAccess (): Nothing = {
			throw new IllegalStateException("Attempting to access config list in an invalid manner")
		}

		override def field(s: String): ConfigValue = illegalAccess()
		override def hasField(s: String): Boolean = false
		override def selectDynamic(methodName: String): ConfigValue = illegalAccess()
		override def fields : Map[String,ConfigValue] = Map()
		override def unwrapped = convertedList.toList
	}


}

object Tmp {
	def main ( args : Array[String] ) {
		val conf = Hocon.parse(
			"""
			  |str : mew
			  |base : {
			  |	a : foo
			  | 	b : bar
			  |}
			  |
			  |wat : ${base}
			  |
			  |array : [
			  |	${base} {
			  | 		b : twobar
			  |   }
			  |
			  |]
			  |
			  |
			""".stripMargin)

		println(conf.array.arr(0).b.str)
	}
}

//object ConfigToSML {
//	def toSML (config : Config) : ConfigValue = {
//		val root = new ConfigValue()
//		for ( entry <- config.entrySet() ; key = entry.getKey ; value = entry.getValue ) {
//
//			root.fields.put(key,toSML(value))
//		}
//		root
//	}
//	def toSML (value : HoconConfigValue) : SMLValue = {
//		import ConfigValueType._
//		new SMLValue(
//			value.valueType() match {
//				case OBJECT => {
//					toSML(value.unwrapped().asInstanceOf[ConfigObject].toConfig)
//				}
//				case LIST => {
//					val clist = value.unwrapped().asInstanceOf[HoconConfigList]
//					val arr = new SMLArray
//					for ( subv <- clist ) {
//						arr.intern :+= toSML(subv)
//					}
//					arr
//				}
//				case _ => value.unwrapped() match {
//					case d : Double => d.toFloat
//					case o => o
//				}
//			}
//		)
//	}
//}
//
//class ConfigWrapper ( config : Config ) extends ConfigValue with Dynamic {
//	override def setSymbolMap(symbolMap: Map[String, Moddable[Float]]): Unit = {}
//
//	override def selectDynamic(methodName: String): SMLValue = {
//		new PartiallyAppliedConfig(config,methodName)
//	}
//}
//
//class PartiallyAppliedConfig ( config : Config , path : String ) extends SMLValue(config.getValue(path)) {
//	val internalValue = config.getValue(path)
//	intern = internalValue.valueType() match {
//		case ConfigValueType.OBJECT => new ConfigWrapper(intern.asInstanceOf[ConfigObject].toConfig)
//		case _ => intern
//	}
//}
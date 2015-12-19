package arx.core.representation

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/7/15
 * Time: 9:33 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.Moddable
import arx.core.function.{ArithmeticExpression, TArithmeticExpression}
import arx.core.vec._
import scalaxy.loops._

class ArxConfigValue(value : Any, _parent : ConfigValue) extends ConfigValue {
	override def parent = _parent
	override def isArr: Boolean = false
	override def isObj: Boolean = false
	
	override def floatOrElse(f: Moddable[Float]): Moddable[Float] = Moddable(float)
	override def floatOrElse(f: Float): Float = float
	override def expressionOrElse(f: Float): TArithmeticExpression = expression
	override def v2OrElse(v: Moddable[ReadVec2f]): Moddable[ReadVec2f] = v2
	override def v2OrElse(v: ReadVec2f): ReadVec2f = v2
	override def strOrElse(s: Moddable[String]): Moddable[String] = Moddable(str)
	override def strOrElse(s: String): String = str
	override def v3OrElse(v: Moddable[ReadVec3f]): Moddable[ReadVec3f] = v3
	override def v3OrElse(v: ReadVec3f): ReadVec3f = v3
	override def v4OrElse(v: ReadVec4f): ReadVec4f = v4
	override def intOrElse(i: Moddable[Int]): Moddable[Int] = Moddable(int)
	override def intOrElse(i: Int): Int = int
	override def boolOrElse(b: Moddable[Boolean]): Moddable[Boolean] = Moddable(bool)
	override def boolOrElse(orElse: Boolean): Boolean = bool
	override def v4OrElse(v: Moddable[ReadVec4f]): Moddable[ReadVec4f] = v4

	override def float: Float = value match {
		case f : Float => f
		case i : Int => i.toFloat
		case d : Double => d.toFloat
		case s : String => ArithmeticExpression.fromString(s).evaluate()
		case o => wasNotType("float"); 0.0f
	}
	override def int: Int = value match {
		case i : Int => i
		case f : Float => Noto.warn("Downcasting to int, not advized"); f.toInt
		case d : Double => Noto.warn("Downcasting to int, not advized"); d.toInt
		case s : String => ArithmeticExpression.fromString(s).evaluate().toInt
	}
	override def bool: Boolean = value match {
		case s : String => s.toBooleanOpt match {
			case Some(b) => b
			case None => Noto.error(f"could not convert $s to boolean"); false
		}
		case b : Boolean => b
		case o => Noto.error(f"Invalid unwrapped type for converting to boolean : $o"); false
	}
	override def expression: TArithmeticExpression = value match {
		case s : String => ArithmeticExpression.fromString(s)
		case f : Float => ArithmeticExpression.fromValue(f)
		case i : Int => ArithmeticExpression.fromValue(i)
		case o =>
			wasNotType("expression")
			ArithmeticExpression.fromValue(0)
	}
	override def arr: ConfigList = {
		wasNotType("array")
		ConfigList.Sentinel
	}
	override def v2: ReadVec2f = { wasNotType("v2"); Vec2f.Zero }
	override def v3: ReadVec3f = { wasNotType("v3"); Vec3f.Zero }
	override def v4: ReadVec4f = { wasNotType("v4"); Vec4f.Zero }
	override def str: String = value.toString
	override def field(s: String): ConfigValue = {
		Noto.error(s"cannot access field on non-object config value, value is $value")
		ConfigValue.Sentinel
	}
	override def hasField(s: String): Boolean = false

	override def isStr: Boolean = value.isInstanceOf[String]
	override def unwrapped: Any = value

	override def selectDynamic(methodName: String): ConfigValue = field(methodName)
	override def fields: Map[String, ConfigValue] = Map()

	protected def wasNotType(to:String) = Noto.error(f"invalid type provided when attempting to convert config value to $to, was : $value")
}

class ArxConfigObject (var children : Map[String,ConfigValue], _parent : ConfigValue) extends ConfigValue {
	var parent = _parent
	override def isArr: Boolean = false
	override def isObj: Boolean = true
	override def isStr: Boolean = false

	override def floatOrElse(f: Moddable[Float]): Moddable[Float] = f
	override def floatOrElse(f: Float): Float = f
	override def expressionOrElse(f: Float): TArithmeticExpression = ArithmeticExpression.fromValue(f)
	override def v2OrElse(v: Moddable[ReadVec2f]): Moddable[ReadVec2f] = v
	override def v2OrElse(v: ReadVec2f): ReadVec2f = v
	override def strOrElse(s: Moddable[String]): Moddable[String] = s
	override def strOrElse(s: String): String = s
	override def v3OrElse(v: Moddable[ReadVec3f]): Moddable[ReadVec3f] = v
	override def v3OrElse(v: ReadVec3f): ReadVec3f = v
	override def v4OrElse(v: ReadVec4f): ReadVec4f = v
	override def intOrElse(i: Moddable[Int]): Moddable[Int] = i
	override def intOrElse(i: Int): Int = i
	override def boolOrElse(b: Moddable[Boolean]): Moddable[Boolean] = b
	override def boolOrElse(orElse: Boolean): Boolean = orElse
	override def v4OrElse(v: Moddable[ReadVec4f]): Moddable[ReadVec4f] = v

	override def float: Float = { wasNotType("float"); 0.0f }
	override def int: Int = { wasNotType("int"); 0 }
	override def bool: Boolean = { wasNotType("bool"); false }
	override def expression: TArithmeticExpression = {
			wasNotType("expression")
			ArithmeticExpression.fromValue(0)
	}
	override def arr: ConfigList = {
		wasNotType("array")
		ConfigList.Sentinel
	}
	override def v2: ReadVec2f = { wasNotType("v2"); Vec2f.Zero }
	override def v3: ReadVec3f = { wasNotType("v3"); Vec3f.Zero }
	override def v4: ReadVec4f = { wasNotType("v4"); Vec4f.Zero }
	override def str: String = { wasNotType("string"); "" }
	override def field(s: String): ConfigValue = {
		children.getOrElse(s,ConfigValue.Sentinel)
	}
	override def hasField(s: String): Boolean = children.contains(s)
	override def unwrapped: Any = children
	override def selectDynamic(methodName: String): ConfigValue = field(methodName)
	override def fields: Map[String, ConfigValue] = children

	protected def wasNotType (t : String): Unit = {
		Noto.error(s"Config Object could not be coerced to type $t")
	}
}

class ArxConfigRoot(chidrin : Map[String,ConfigValue], val uid : Int) extends ArxConfigObject(chidrin,ConfigValue.Sentinel) with ConfigRoot {
	override def isRoot : Boolean = true
}

class ArxConfigList (var values : Seq[ConfigValue], _parent : ConfigValue) extends ConfigValue with ConfigList {
	override def parent = _parent
	override def isArr: Boolean = true
	override def isObj: Boolean = false
	override def isStr: Boolean = false

	override def floatOrElse(f: Moddable[Float]): Moddable[Float] = f
	override def floatOrElse(f: Float): Float = f
	override def expressionOrElse(f: Float): TArithmeticExpression = ArithmeticExpression.fromValue(f)
	override def v2OrElse(v: Moddable[ReadVec2f]): Moddable[ReadVec2f] = v
	override def v2OrElse(v: ReadVec2f): ReadVec2f = v
	override def strOrElse(s: Moddable[String]): Moddable[String] = s
	override def strOrElse(s: String): String = s
	override def v3OrElse(v: Moddable[ReadVec3f]): Moddable[ReadVec3f] = v
	override def v3OrElse(v: ReadVec3f): ReadVec3f = v
	override def v4OrElse(v: ReadVec4f): ReadVec4f = v
	override def intOrElse(i: Moddable[Int]): Moddable[Int] = i
	override def intOrElse(i: Int): Int = i
	override def boolOrElse(b: Moddable[Boolean]): Moddable[Boolean] = b
	override def boolOrElse(orElse: Boolean): Boolean = orElse
	override def v4OrElse(v: Moddable[ReadVec4f]): Moddable[ReadVec4f] = v

	override def float: Float = { wasNotType("float"); 0.0f }
	override def int: Int = { wasNotType("int"); 0 }
	override def bool: Boolean = { wasNotType("bool"); false }
	override def expression: TArithmeticExpression = {
		wasNotType("expression")
		ArithmeticExpression.fromValue(0)
	}
	override def arr: ConfigList = { this }
	override def v2: ReadVec2f = Vec2f(values(0).float,values(1).float)
	override def v3: ReadVec3f = Vec3f(values(0).float,values(1).float,values(2).float)
	override def v4: ReadVec4f = Vec4f(values(0).float,values(1).float,values(2).float,values(3).float)
	override def str: String = { wasNotType("string"); "" }
	override def field(s: String): ConfigValue = { wasNotType("object"); ConfigValue.Sentinel }
	override def hasField(s: String): Boolean = false
	override def unwrapped: Any = values
	override def selectDynamic(methodName: String): ConfigValue = field(methodName)
	override def fields: Map[String, ConfigValue] = Map()

	override def apply (i : Int) = values(i)
	override def foreach[U](f: (ConfigValue) => U): Unit = values.foreach(f)
	override def size = values.size
	override def isEmpty = values.isEmpty
	override def nonEmpty = !isEmpty

	protected def wasNotType (t : String): Unit = {
		Noto.error(s"Config Object could not be coerced to type $t")
	}
}
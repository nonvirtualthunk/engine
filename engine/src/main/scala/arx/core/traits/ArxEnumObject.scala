package arx.core.traits

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/14/14
 * Time: 8:13 AM
 */

class ArxEnumObject[T <: TArxEnum : Manifest] {
	def exists ( str : String ) : Boolean = TArxEnum.existing[T](str.toLowerCase).nonEmpty
	def apply ( str : String ) : T = {
		TArxEnum.existing[T](str.toLowerCase) match {
			case Some(f) => f
			case None => manifest[T].runtimeClass.getConstructor(classOf[String]).newInstance(str).asInstanceOf[T]
		}
	}
}

abstract class ArxEnumObjectWithDefault[T <: ArxEnum : Manifest] {
	def defaultValue : T
	def exists ( str : String ) : Boolean = TArxEnum.existing[T](str.toLowerCase).nonEmpty
	def apply ( str : String ) : T = {
		TArxEnum.existing[T](str.toLowerCase) match {
			case Some(f) => f
			case None =>
				println(s"Enum (${manifest[T].runtimeClass.getSimpleName}, value not found : $str")
				defaultValue
		}
	}
}
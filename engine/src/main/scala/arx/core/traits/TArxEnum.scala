package arx.core.traits

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 4/13/13
 * Time: 12:42 PM
 * Created by nonvirtualthunk
 */

import java.io.ObjectInputStream
import java.io.ObjectOutputStream

import arx.Prelude._
import arx.application.Noto
import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.KryoSerializable
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output
import scala.collection.mutable

@SerialVersionUID(1L)
trait TArxEnum extends Serializable {
	def key : Any

	def readResolve : Object = {
		TArxEnum.enumsByClassAndKey.get(this.getClass -> key) match {
			case Some(e) => e
			case None =>
				TArxEnum.enumsByClassAndKey += (this.getClass -> key) -> this
				this
		}
	}

	override def equals ( other : Any ) = other match {
		case ar : TArxEnum => this.getClass == ar.getClass && this.key == ar.key
		case _ => false
	}
	override def hashCode = key.hashCode


	TArxEnum.enumsByClassAndKey += (this.getClass -> key) -> this

	override def toString: String = key match {
		case a : AnyRef => a.toString
		case o : Any => super.toString
	}
}

object TArxEnum {
	val enumsByClassAndKey = new mutable.HashMap[(Class[_],Any),TArxEnum]

	def existing[T <: TArxEnum : Manifest] ( key : Any ) =
		enumsByClassAndKey.get(manifest[T].runtimeClass -> key).asInstanceOf[Option[T]]

	def withKey[T <: TArxEnum : Manifest] ( key : Any , orElse : T ) = {
		enumsByClassAndKey.get(manifest[T].runtimeClass -> key).asInstanceOf[Option[T]].getOrElse{
			Noto.warn(s"requested enum with key '$key' of type ${manifest[T].runtimeClass.getSimpleName}, but none such exists")
			orElse
		}
	}
}

class ArxEnum(val name : String) extends TArxEnum {
	def key = name.toLowerCase
}
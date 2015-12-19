package arx.core.datastructures.primitive

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/24/15
 * Time: 8:58 PM
 */

import arx.Prelude._
import com.esotericsoftware.kryo.io.{Input, Output}
import scalaxy.loops._

object Primitive {
	val BytePrim = 0
	val ShortPrim = 1
	val IntPrim = 2
	val LongPrim = 3
	val FloatPrim = 4
	val DoublePrim = 5


	def primId (man : Manifest[_]) = {
		if (man == manifest[Byte]) {
			BytePrim
		} else if (man == manifest[Short]) {
			ShortPrim
		} else if (man == manifest[Int]) {
			IntPrim
		} else if (man == manifest[Long]) {
			LongPrim
		} else if (man == manifest[Float]) {
			FloatPrim
		} else if (man == manifest[Double]) {
			DoublePrim
		} else {
			-1
		}
	}

	def manifestFor (primId : Int) : Manifest[_ <: AnyVal] = {
		primId match {
			case BytePrim => manifest[Byte]
			case ShortPrim => manifest[Short]
			case IntPrim => manifest[Int]
			case LongPrim => manifest[Long]
			case FloatPrim => manifest[Float]
			case DoublePrim => manifest[Double]
		}
	}

	def writePrim (output : Output, primId : Int, value : Any): Unit = {
		primId match {
			case BytePrim => output.writeByte(value.asInstanceOf[Byte])
			case ShortPrim => output.writeShort(value.asInstanceOf[Short])
			case IntPrim => output.writeInt(value.asInstanceOf[Int])
			case LongPrim => output.writeLong(value.asInstanceOf[Long])
			case FloatPrim => output.writeFloat(value.asInstanceOf[Float])
			case DoublePrim => output.writeDouble(value.asInstanceOf[Double])
		}
	}
	def readPrim (input : Input, primId : Int) : AnyVal = {
		primId match {
			case BytePrim => input.readByte
			case ShortPrim => input.readShort
			case IntPrim => input.readInt
			case LongPrim => input.readLong
			case FloatPrim => input.readFloat
			case DoublePrim => input.readDouble
		}
	}
}

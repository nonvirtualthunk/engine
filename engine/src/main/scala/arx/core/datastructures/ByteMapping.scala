package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/18/13
 * Time: 10:50 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.traits.TIdentifiable
import scala.collection.mutable

class ByteMapping[T <: TIdentifiable] ( sentinelValue : T )(implicit man : Manifest[T])  {
	val forward = new mutable.HashMap[String,Byte]
	var backward = new GrowableArray[T]()(man)

	this.apply(sentinelValue)

	def apply ( t : T ) = {
		forward.getOrElseUpdate(t.identifier,{
			backward.append(t)
			(backward.size - 1).toByte
		})
	}

	def set ( b : Byte , t : T ) = {
		val setTo = forward.getOrElseUpdate(t.identifier,{
			val i = b & 0xff
			backward.ensureSize(i+1)
			backward(i) = t
			i.toByte
		})
		if ( setTo != b ) { Noto.warn("ByteMappting.set(...) called, but reference already existed at different index") }
		setTo
	}

	def apply ( b : Byte ) = {
		backward(b & 0xff)
	}
	def apply ( b : Int ) = {
		backward(b)
	}

	def size = backward.size

	def toList = backward.toList

	def typeManifest = man
}

class ShortMapping[T <: TIdentifiable] ( sentinelValue : T )(implicit man : Manifest[T])  {
	val forward = new mutable.HashMap[String,Short]
	var backward = new GrowableArray[T]()(man)

	this.apply(sentinelValue)

	def apply ( t : T ) = {
		forward.getOrElseUpdate(t.identifier,{
			backward.append(t)
			(backward.size - 1).toShort
		})
	}

	def set ( b : Byte , t : T ) = {
		val setTo = forward.getOrElseUpdate(t.identifier,{
			val i = b & 0xffff
			backward.ensureSize(i+1)
			backward(i) = t
			i.toShort
		})
		if ( setTo != b ) { Noto.warn("ByteMappting.set(...) called, but reference already existed at different index") }
		setTo
	}

	def apply ( b : Byte ) = {
		backward(b & 0xff)
	}
	def apply ( b : Short ) = {
		backward(b & 0xffff)
	}
	def apply ( b : Int ) = {
		backward(b)
	}

	def size = backward.size

	def toList = backward.toList

	def typeManifest = man
}
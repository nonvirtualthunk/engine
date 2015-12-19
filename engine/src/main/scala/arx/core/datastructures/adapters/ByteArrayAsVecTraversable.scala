package arx.core.datastructures.adapters

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/19/14
 * Time: 7:55 AM
 */

import arx.Prelude._
import arx.core.datastructures.primitive.ByteArray2D
import arx.core.traits.TArxTraversable
import arx.core.vec.{Vec2i, ReadVec2i}
import scalaxy.loops._

class ByteArrayAsVecTraversable(val arr : ByteArray2D) extends TArxTraversable[ReadVec2i] {
	override def foreach[U](f: (ReadVec2i) => U): Unit = {
		for (x <- 0 until arr.width optimized ; y <- 0 until arr.height optimized) {
			if (arr(x,y) != 0) {
				f(ReadVec2i(x,y))
			}
		}
	}

	override def foreachUnsafe[U](f: (ReadVec2i) => U): Unit = {
		val v = Vec2i(0,0)
		for (x <- 0 until arr.width optimized ; y <- 0 until arr.height optimized) {
			if (arr(x,y) != 0) {
				v.x = x
				v.y = y
				f(v)
			}
		}
	}
}
package arx.core.datastructures.adapters

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/19/14
 * Time: 8:26 AM
 */

import arx.Prelude._
import arx.core.traits.TArxTraversable
import arx.core.vec.{Vec2i, ReadVec2i}
import scalaxy.loops._

class BooleanFunctionAsVecTraversable(func : (Int,Int) => Boolean, width:Int,height:Int) extends TArxTraversable[ReadVec2i] {
	override def foreach[U](f: (ReadVec2i) => U): Unit = {
		for (x <- 0 until width optimized ; y <- 0 until height optimized) {
			if (func(x,y)) {
				f(ReadVec2i(x,y))
			}
		}
	}

	override def foreachUnsafe[U](f: (ReadVec2i) => U): Unit = {
		val v = Vec2i(0,0)
		for (x <- 0 until width optimized ; y <- 0 until height optimized) {
			if (func(x,y)) {
				v.x = x
				v.y = y
				f(v)
			}
		}
	}
}

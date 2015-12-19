package arx.core.datastructures.primitive

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/19/14
 * Time: 8:05 AM
 */

import java.io.{Externalizable, ObjectInput, ObjectOutput}
import java.util

class IntArray2D(var width:Int,var height:Int) extends Externalizable {
	def this () { this(1,1) }
	import scalaxy.loops._
	var intern = Array.ofDim[Int](width * height)
	def apply(x:Int,y:Int) : Int = {
		intern(x * height + y)
	}
	def update(x:Int,y:Int,b:Int): Unit = {
		intern(x * height + y) = b
	}
	def setAll (b : Int) {
		util.Arrays.fill(intern,b)
	}

	override def writeExternal(p1: ObjectOutput): Unit = {
		p1.writeInt(width)
		p1.writeInt(height)
		for (x <- 0 until width optimized ; y <- 0 until height) {
			p1.writeInt(this.apply(x,y))
		}
	}
	override def readExternal(p1: ObjectInput): Unit = {
		width = p1.readInt
		height = p1.readInt
		intern = Array.ofDim[Int](width * height)
		for (x <- 0 until width optimized ; y <- 0 until height) {
			this(x,y) = p1.readInt
		}
	}
}


class ShortArray2D(var width:Int,var height:Int) extends Externalizable {
	def this () { this(1,1) }
	import scalaxy.loops._
	var intern = Array.ofDim[Short](width * height)
	def apply(x:Int,y:Int) : Short = {
		intern(x * height + y)
	}
	def update(x:Int,y:Int,b:Short): Unit = {
		intern(x * height + y) = b
	}
	def setAll (b : Short) {
		util.Arrays.fill(intern,b)
	}

	override def writeExternal(p1: ObjectOutput): Unit = {
		p1.writeInt(width)
		p1.writeInt(height)
		for (x <- 0 until width optimized ; y <- 0 until height) {
			p1.writeShort(this.apply(x,y))
		}
	}
	override def readExternal(p1: ObjectInput): Unit = {
		width = p1.readInt
		height = p1.readInt
		intern = Array.ofDim[Short](width * height)
		for (x <- 0 until width optimized ; y <- 0 until height) {
			this(x,y) = p1.readShort
		}
	}
}

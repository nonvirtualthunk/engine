package arx.core.datastructures.primitive

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/19/14
 * Time: 8:04 AM
 */

import java.io.{Externalizable, ObjectInput, ObjectOutput}
import java.util

import arx.core.datastructures.FiniteGrid2D
import arx.core.vec.{Vec2i, ReadVec2i}

class ByteArray2D(var width:Int,var height:Int) extends Externalizable with FiniteGrid2D[Byte] {

	val minimumPoint: ReadVec2i = Vec2i.Zero
	val maximumPoint: ReadVec2i = Vec2i(width-1,height-1)

	def this () { this(1,1) }
	import scalaxy.loops._
	var intern = Array.ofDim[Byte](width * height)
	def apply(x:Int,y:Int) : Byte = {
		intern(x * height + y)
	}
	def update(x:Int,y:Int,b:Byte): Unit = {
		intern(x * height + y) = b
	}
	def setAll (b : Byte) {
		util.Arrays.fill(intern,b)
	}

	override def writeExternal(p1: ObjectOutput): Unit = {
		p1.writeInt(width)
		p1.writeInt(height)
		for (x <- 0 until width optimized ; y <- 0 until height) {
			p1.writeByte(this.apply(x,y))
		}
	}
	override def readExternal(p1: ObjectInput): Unit = {
		width = p1.readInt
		height = p1.readInt
		intern = Array.ofDim[Byte](width * height)
		for (x <- 0 until width optimized ; y <- 0 until height) {
			this(x,y) = p1.readByte
		}
	}

	@inline
	override def contains(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
}

package arx.core.datastructures.voxel

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/8/16
  * Time: 6:55 AM
  */

import java.util

import arx.Prelude._
import scalaxy.loops._
import arx.core.vec._

@SerialVersionUID(1L)
class TaleaDataContainer[@specialized(Byte,Short,Int) T](val dimension:Int, val defaultValue:T, val componentClass : Class[T]) {
	var dimensionPo2:Int = (math.log(dimension) / math.log(2)).toInt
	var dimensionPo22:Int = dimensionPo2 + dimensionPo2
	private[datastructures] var data: Array[T] = {
		val d = componentClass.newArray(1)
		d(0) = defaultValue
		d
	}
	var _mask : Int = 0x00000000
	def mask : Int = _mask


	def rawData = data
	def setData (d : Array[T]) { data = d}


	def compressed: Boolean = false

	@volatile def allocate (){
		data = componentClass.newArray(dimension * dimension * dimension)
		_mask = 0xffffffff
	}
	@volatile def allocate (defaultValue : T) {
		val tmp = componentClass.newArray(dimension * dimension * dimension)
		//If defaultValue is not the built-in default
		if ( tmp(0) != defaultValue ) {
			val size = dimension * dimension * dimension

			tmp(0) = defaultValue
			var i = 1
			while (i < size) {
				val copyLen = if (size - i < i) {
					size - i
				} else {
					i
				}
				System.arraycopy(tmp,0,tmp,i,copyLen)
				i += i
			}
		}

		data = tmp
		_mask = 0xffffffff
	}
	def deallocate (){
		_mask = 0x00000000
		val tmp = componentClass.newArray(1)
		tmp(0) = defaultValue
		data = tmp
	}

	def compress (keepOriginal: Boolean = false){
		throw new IllegalStateException("Wrong exception, point is we don't compress/uncompress these days")
	}
	def uncompress (){
		throw new IllegalStateException("Wrong exception, point is we don't compress/uncompress these days")
	}


	@inline def toIndex ( x : Int , y : Int , z : Int ) = (z << dimensionPo22) + (y << dimensionPo2) + x
	//Test fromIndex, not sure how correct this is
	@inline def fromIndex ( i : Int ) = Vec3i( i & (dimension-1) , (i - ((i >> dimensionPo22) << dimensionPo22)) >> dimensionPo2 , i >> dimensionPo22  )
	@inline def zFromIndex ( i : Int ) : Int = i >> dimensionPo22
	@inline def xFromIndex ( i : Int ) : Int = i & (dimension-1)
	@inline def yFromIndex ( i : Int ) : Int = (i - ((i >> dimensionPo22) << dimensionPo22)) >> dimensionPo2

	@inline def apply (x: Int, y: Int, z: Int): T = data(((z << dimensionPo22) + (y << dimensionPo2) + x) & mask)
	@inline def update (x: Int,y: Int,z: Int,b: T){ data((z << dimensionPo22) + (y << dimensionPo2) + x) = b }

	@inline def getUnsafe(x: Int, y: Int, z: Int): T = data((z << dimensionPo22) + (y << dimensionPo2) + x)
	@inline def getByIndexUnsafe(i:Int) = data(i)
	@inline def getByIndex(i:Int) = data(i & mask)
	@inline def setByIndex(i:Int,b:T) { data(i) = b }

	def loadRow(startX : Int, startY : Int, startZ : Int, xLength : Int, startOff : Int, out : Array[T]) = {
		val arr = data
		if (arr.length == 1) {
//			for (i <- startOff until startOff + xLength optimized) {
//				out(i) = defaultValue
//			}
			out(startOff) = defaultValue
			var i = 1
			while (i < xLength) {
				val copyLen = if (xLength - i < i) {
					xLength - i
				} else {
					i
				}
				System.arraycopy(out,startOff,out,startOff+i,copyLen)
				i += i
			}
		} else {
			val index = toIndex(startX,startY,startZ)
			System.arraycopy(arr,index,out,startOff,xLength)
		}
	}

	def storeRow(startY: Int, startZ: Int, in: Array[T], startOff : Int): Unit = {
		val index = toIndex(0,startY,startZ)
		System.arraycopy(in,startOff,data,index,dimension)
	}

	def getBlock2x2(x : Int,y: Int,z: Int,ret : Array[T]) {
		val h1 = (z << dimensionPo22) + (y << dimensionPo2) + x
		ret(0) = data(h1 & mask)
		ret(1) = data((h1 + 1) & mask)
		ret(2) = data((h1 + dimension) & mask)
		ret(3) = data((h1 + dimension + 1) & mask)
		val h2 = ((z + 1) << dimensionPo22) + (y << dimensionPo2) + x
		ret(4) = data(h2 & mask)
		ret(5) = data((h2 + 1) & mask)
		ret(6) = data((h2 + dimension) & mask)
		ret(7) = data((h2 + dimension + 1) & mask)
	}

	def memoryUsage: Int = {
		var sum = 0
		if ( data != null ){ sum += data.length }
		sum
	}

	override def equals ( a : Any ) : Boolean = {
		a match {
			case d : TaleaDataContainer[T] => {
				this.dimension == d.dimension &&
					this.mask == d.mask &&
					(if ( this.data == null ) { d.rawData == null } else { (0 until this.data.length).forall( { i => this.data(i) == d.rawData(i) } ) })
			}
			case _ => false
		}
	}
}
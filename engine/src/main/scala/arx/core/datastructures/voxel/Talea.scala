package arx.core.datastructures.voxel

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/7/16
  * Time: 4:22 PM
  */

import arx.Prelude
import arx.Prelude._
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.vec._
import arx.core.vec.coordinates.VoxelCoord


class Talea[@specialized(Byte,Short,Int) T](val _position : VoxelCoord, var _defaultValue: T) extends TTalea[T] {
	hash = Talea.hash(_position.x,_position.y,_position.z)
	protected[datastructures] var _size = Talea.dimension

	def createDataContainer() = new TaleaDataContainer[T](Talea.dimension,_defaultValue,getComponentType.asInstanceOf[Class[T]])

	protected var _nonDefaultCount: Int = 0
	protected var _shiftedPosition: ReadVec3i = _position >> Talea.dimensionPo2

	def position = _position
	def size = _size
	def shiftedPosition : ReadVec3i = _shiftedPosition
	var data : TaleaDataContainer[T] = createDataContainer()
	def nonDefaultCount : Int = _nonDefaultCount
	def defaultValue = _defaultValue

	override def subStore(region: VoxelRegion): VoxelStore[T] with BoundedVoxelView[T] = ???
	override def subView(region: VoxelRegion): VoxelView[T] with BoundedVoxelView[T] = ???

	protected[datastructures] var _loggedModifications = List[ LoggedVoxelModification[T] ]()
	def loggedModifications = _loggedModifications


	protected[datastructures] def getComponentType = _defaultValue.getClass
	protected[datastructures] def withData (d : TaleaDataContainer[T]) = { data = d ; this }

	override def equals ( a : Any ) : Boolean = {
		a match {
			case t: Talea[T] => {
				t.position.equals(position) &&
					t._defaultValue == this._defaultValue &&
					t.nonDefaultCount == this.nonDefaultCount &&
					t.shiftedPosition.equals(this.shiftedPosition) &&
					t._modifiedCount == this._modifiedCount &&
					t.hash == this.hash &&
					t.data.equals(this.data)
			}
			case _ => false
		}
	}


	override def hashCode() = hash

	def apply (x: Int,y: Int,z: Int): T = {
		data(x,y,z)
	}

	def loadRow(startX : Int, startY : Int, startZ : Int, xLength : Int, startOff : Int, out : Array[T]) = {
		data.loadRow(startX,startY,startZ,xLength,startOff,out)
	}

	def getAndDecrementToMinimumOf(x: Int, y: Int, z: Int,minimumValue : T) = throw new UnsupportedOperationException

	def setIfEqual ( x : Int, y : Int,z : Int, curV : T , newV : T ) : T = {
		val actual = this(x,y,z)
		if ( actual == curV ) {
			this(x,y,z) = newV
		}
		actual
	}

	def setIfNotEqual ( x : Int , y : Int , z : Int , curV : T , newV : T ) : T = {
		val actual = this(x,y,z)
		if ( actual != curV ) {
			this(x,y,z) = newV
		}
		actual
	}

	def setIfEqualRaw ( x : Int ,y  : Int,z : Int, curV : T , b : T ) : T = {
		val actual = this(x,y,z)
		if ( actual == curV && curV != b) {
			if ( _nonDefaultCount == 0 ){
				if ( b != _defaultValue ){
					_nonDefaultCount += 1

					if ( data.mask == 0x00000000 ) {
						val newData = createDataContainer()
						newData.allocate(_defaultValue)
						data = newData
					}

					data(x,y,z) = b
					markModified(x,y,z)
				}
			}
			else{
				if ( curV == _defaultValue ){
					_nonDefaultCount += 1
				}
				else if ( b == _defaultValue ){
					_nonDefaultCount -= 1
				}

				if ( _nonDefaultCount == 0 ){
					data = createDataContainer()
					data.rawData(0) = _defaultValue
				} else {
					data(x,y,z) = b
				}

				markModified(x,y,z)
			}
		}
		actual
	}


	override def setAll(b: T): Unit = {
		synchronized {
			_defaultValue = b
			_nonDefaultCount = 0
			data.deallocate()
			_modifiedCount += 1
			_edgeModifiedCount = _edgeModifiedCount.map ( _ + 1 )
		}
	}

	def setIfGreater ( x : Int , y : Int, z : Int, newV : T ) : T = {
		throw new UnsupportedOperationException("Java stupid generics means we can't use <, >, etc in the generic talea")
	}

	def getBlock2x2 (x : Int,y : Int,z : Int,ret : Array[T]) {
		if ( nonDefaultCount != 0 ) {
			data.getBlock2x2(x,y,z,ret)
		} else {
			ret(0) = _defaultValue
			ret(1) = _defaultValue
			ret(2) = _defaultValue
			ret(3) = _defaultValue
			ret(4) = _defaultValue
			ret(5) = _defaultValue
			ret(6) = _defaultValue
			ret(7) = _defaultValue
		}
	}
	def getWithAdj (x:Int,y:Int,z:Int,ret : Array[T]) {
		throw new UnsupportedOperationException
	}

	def update (x: Int,y: Int,z: Int,b: T){
		Prelude.posit( x >= 0 && x < Talea.dimension && y >= 0 && y < Talea.dimension && z >= 0 && z < Talea.dimension , "Update called with invalid bounds" )
		synchronized{
			if ( _nonDefaultCount == 0 ){
				if ( b != _defaultValue ){
					_nonDefaultCount += 1

						if ( data.mask == 0x00000000 ) {
						data.allocate(defaultValue)
					}

					data(x,y,z) = b
					markModified(x,y,z)
					if ( isLoggingEnabled ) { _loggedModifications ::= LoggedVoxelModification(x,y,z,_defaultValue,b,_modifiedCount) }
				}
			}
			else{
				val index = data.toIndex(x,y,z)
				val cur = data.getByIndexUnsafe(index)
				if ( b != cur ){
					if ( cur == _defaultValue ){
						_nonDefaultCount += 1
					}
					else if ( b == _defaultValue ){
						_nonDefaultCount -= 1
					}

					if ( _nonDefaultCount == 0 ){
						data = createDataContainer()
					}
					else{
						data.setByIndex(index,b)
					}

					markModified(x,y,z)
					if ( isLoggingEnabled ) { _loggedModifications ::= LoggedVoxelModification(x,y,z,cur,b,_modifiedCount) }
				}
			}
		}
	}

	override def setUnsafe (x: Int,y: Int,z: Int,b: T){
		if ( _nonDefaultCount == 0 ){
			if ( b != _defaultValue ){
				_nonDefaultCount += 1

				if ( data.mask == 0x00000000 ) {
					data.allocate(defaultValue)
				}

				data(x,y,z) = b
				markModified(x,y,z)
				if ( isLoggingEnabled ) { _loggedModifications ::= LoggedVoxelModification(x,y,z,_defaultValue,b,_modifiedCount) }
			}
		}
		else{
			val index = data.toIndex(x,y,z)
			val cur = data.getByIndexUnsafe(index)
			if ( b != cur ){
				if ( cur == _defaultValue ){
					_nonDefaultCount += 1
				}
				else if ( b == _defaultValue ){
					_nonDefaultCount -= 1
				}

				if ( nonDefaultCount == 0 ){
					data = createDataContainer()
				}
				else{
					data.setByIndex(index,b)
				}

				markModified(x,y,z)
				if ( isLoggingEnabled ) { _loggedModifications ::= LoggedVoxelModification(x,y,z,cur,b,_modifiedCount) }
			}
		}
	}

	def markModified ( i : Int ) {
		val v = data.fromIndex(i)
		markModified(v.x,v.y,v.z)
	}

	def markModified ( x : Int , y : Int , z : Int ) {
		_modifiedCount += 1
		if ( x == 0 ) { _edgeModifiedCount(Cardinals.Left) += 1 } else if ( x == Talea.dimensionM1 ) { _edgeModifiedCount(Cardinals.Right) += 1 }
		if ( y == 0 ) { _edgeModifiedCount(Cardinals.Back) += 1 } else if ( y == Talea.dimensionM1 ) { _edgeModifiedCount(Cardinals.Front) += 1 }
		if ( z == 0 ) { _edgeModifiedCount(Cardinals.Bottom) += 1 } else if ( z == Talea.dimensionM1 ) { _edgeModifiedCount(Cardinals.Top) += 1 }
	}

	def areAll (b: T): Boolean = { nonDefaultCount == 0 && _defaultValue == b }
	def areNone (b: T): Boolean = { _defaultValue == b && nonDefaultCount == Talea.dimension*Talea.dimension*Talea.dimension }

	override def memoryUsage: Int = {
		data.memoryUsage
	}



	def getShiftedPosition = shiftedPosition

	override def containsPoint ( v : ReadVec3i ) : Boolean = {
		v.x >= position.x && v.y >= position.y && v.z >= position.z &&
			v.x < position.x + _size && v.y < position.y + _size && v.z < position.z + _size
	}


	def foreach(f: (Int, Int, Int, T) => Unit): Unit = {
		var x = 0
		var y = 0
		var z = 0
		var index = 0
		val maxIndex = _size*_size*_size
		while(index < maxIndex) {
			val v = data.getByIndex(index)
			f(x,y,z,v)
			index += 1
			x += 1
			y += (x / _size)
			z += (y / _size)
			x %= _size
			y %= _size
		}
	}

	override def toString : String = {
		"Talea " + position
	}
}




object Talea {
	val dimensionPo2 = 5//:Int = (Math.log(dimension) / Math.log(2)).toInt
	val dimension = 1 << dimensionPo2
	val dimensionf = dimension.toFloat
	val dimensionM1 = dimension - 1

	def hash (x: Int,y: Int,z: Int): Int = {
		((x >> dimensionPo2) << 20) | ((y >> dimensionPo2) << 10) | (z >> dimensionPo2)
	}
	def hashPreShifted(x: Int, y: Int, z: Int): Int = {
		( (x) << 20) | ((y) << 10) | (z)
	}
}
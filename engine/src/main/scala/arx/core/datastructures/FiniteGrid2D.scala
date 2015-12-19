package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/18/13
 * Time: 10:28 AM
 * Created by nonvirtualthunk
 */


import arx.Prelude._
import arx.application.Noto
import arx.core.vec._
import arx.core.vec.coordinates.VoxelCoord
import scalaxy.loops._

trait FiniteGrid2D[T] extends Serializable with Traversable[T] with Function2[Int,Int,T]{
	@inline
	def apply ( x : Int , y : Int ) : T
	@inline
	def apply ( v : VoxelCoord ) : T = apply(v.x,v.y)
	@inline
	def apply ( v : ReadVec2i ) : T = apply(v.x,v.y)

	@inline
	def contains( x : Int,y: Int) : Boolean
	@inline
	def contains( v : ReadVec2i ) : Boolean = contains(v.x,v.y)

	def minimumPoint : ReadVec2i
	def maximumPoint : ReadVec2i


	def width : Int
	def height : Int
	def dimensions : ReadVec2i = Vec2i(width,height)


	override def foreach[U](f: (T) => U): Unit = {
		val min = minimumPoint
		val max = maximumPoint
		for (x <- min.x to max.x optimized ; y <- min.y to max.y optimized) {
			f(this(x,y))
		}
	}

	def forPositionsWhere[U] ( condition : (T) => Boolean )( func : (ReadVec2i) => U ): Unit = {
		val min = minimumPoint
		val max = maximumPoint
		for (x <- min.x to max.x optimized ; y <- min.y to max.y optimized) {
			if ( condition(this(x,y)) ) { func(ReadVec2i(x,y))}
		}
	}

	def positionsWhere (condition : (T) => Boolean) = {
		var ret = Vector[ReadVec2i]()
		forPositionsWhere(condition) { v =>
			ret :+= v
		}
		ret
	}

	def subsectionWithCorners (min : ReadVec2i, max : ReadVec2i) = subsection(min,max - min + 1)

	def subsection (position : ReadVec2i, dims : ReadVec2i) = {
		val self = this
		new FiniteGrid2D[T] {
			val minimumPoint: ReadVec2i = VoxelCoord(position.x,position.y,0)
			val maximumPoint: ReadVec2i = VoxelCoord(position.x + dims.x - 1,position.y + dims.y - 1,0)
			@inline override def apply(x: Int, y: Int): T = self.apply(x,y)
			@inline override def contains(x: Int, y: Int): Boolean = self.contains(x,y) && x >= minimumPoint.x && x <= maximumPoint.x && y >= minimumPoint.y && y <= maximumPoint.y
			override def height: Int = dims.y
			override def width: Int = dims.x
		}
	}
}

trait WriteableFiniteGrid2D[T] extends FiniteGrid2D[T] {
	@inline
	def update ( x : Int , y : Int , t : T )
	@inline
	def update ( v : VoxelCoord , t : T ) { update(v.x,v.y,t) }
	@inline
	def update ( v : ReadVec2i , t : T ) { update(v.x,v.y,t) }

	def _setFromFunction ( f : (Int,Int) => T ) {
		val min = this.minimumPoint
		val max = this.maximumPoint
		for (x <- min.x to max.x optimized ; y <- min.y to max.y optimized) {
				this(x,y) = f(x,y)
		}

	}
}

@SerialVersionUID(1L)
class Po2FiniteGrid2D[T] ( basePoint : ReadVec2i, twoToTheNDims : ReadVec2i, dims : ReadVec2i, backingArray : Array[T], sentinelValue : T ) extends WriteableFiniteGrid2D[T] {
	for (i <- 0 until backingArray.length optimized) { backingArray(i) = sentinelValue }
	private final val minorShift = twoToTheNDims.x

	private final val ox = basePoint.x //origin is at voxel coord center minus half the dims
	private final val oy = basePoint.y

	private final val mx = ox + dims.x
	private final val my = oy + dims.y

	val minimumPoint = basePoint
	val maximumPoint = ReadVec2i(mx-1,my-1)

	override def width = dims.x
	override def height = dims.y
	override def dimensions = dims

	@inline
	def contains( x : Int,y: Int) : Boolean = x >= ox && y >= oy && x < mx && y < my

	@inline
	private final def index ( x : Int , y : Int ) = ((y - oy) << minorShift) + (x - ox)

	@inline
	private final def checkIndices ( x : Int , y : Int ) = {
		posit(x >= ox && y >= oy && x < mx && y < my,
			f"Out of bounds in finite two grid, ($x,$y), dims are $dims")
	}

	def apply(x: Int, y: Int): T = {
		if ( contains(x,y) ) {
			backingArray( index(x,y) )
		} else {
			sentinelValue
		}
	}

	def update(x: Int, y: Int, t: T) {
		if ( contains(x,y) ) {
			backingArray( index(x,y) ) = t
		}
	}
}

@SerialVersionUID(1L)
class SimpleFiniteGrid2D[T]( basePoint : ReadVec2i, dims : ReadVec2i, backingArray : Array[T], sentinelValue : T ) extends WriteableFiniteGrid2D[T] {
	for (i <- 0 until backingArray.length optimized) { backingArray(i) = sentinelValue }
	private final val ox = basePoint.x //origin is at voxel coord center minus half the dims
	private final val oy = basePoint.y

	private final val mx = ox + dims.x
	private final val my = oy + dims.y

	val minimumPoint = basePoint
	val maximumPoint = ReadVec2i(mx-1,my-1)

	override def width = dims.x
	override def height = dims.y
	override def dimensions = dims

	@inline
	def contains( x : Int,y: Int) : Boolean = x >= ox && y >= oy && x < mx && y < my

	@inline
	private final def index ( x : Int , y : Int ) = ((y - oy) * dims.x) + (x - ox)

	@inline
	private final def checkIndices ( x : Int , y : Int ) = {
		posit(x >= ox && y >= oy && x < mx && y < my,
			f"Out of bounds in finite two grid, ($x,$y), dims are $dims")
	}

	def apply(x: Int, y: Int): T = {
		if ( contains(x,y) ) {
			backingArray( index(x,y) )
		} else {
			sentinelValue
		}
	}

	def update(x: Int, y: Int, t: T) {
		if ( contains(x,y) ) {
			backingArray( index(x,y) ) = t
		}
	}
}

class FunctionBackedGrid[T] ( dims : ReadVec2i , func : (Int,Int) => T ) extends FiniteGrid2D[T] {

	override val minimumPoint: ReadVec2i = Vec2i.Zero
	override val maximumPoint: ReadVec2i = dims - 1

	@inline
	override def apply(x: Int, y: Int): T = func(x,y)
	@inline
	override def contains(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < dims.x && y < dims.y

	override def width: Int = dims.x
	override def height: Int = dims.y
	override def dimensions = dims
}

class PositionedFunctionBackedGrid[T] ( position : ReadVec2i, dims : ReadVec2i , func : (Int,Int) => T ) extends FiniteGrid2D[T] {

	val minimumPoint: ReadVec2i = position
	val maximumPoint: ReadVec2i = ReadVec2i(position.x + dims.x - 1,position.y + dims.y - 1)

	@inline
	override def apply(x: Int, y: Int): T = func(x,y)
	@inline
	override def contains(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < dims.x && y < dims.y

	override def width: Int = dims.x
	override def height: Int = dims.y
	override def dimensions = dims
}

object FiniteGrid2D {
	def apply[T : Manifest] ( dimensions : ReadVec2i, sentinel : T ) : WriteableFiniteGrid2D[T] = {
		this.apply[T](Vec2i.Zero,dimensions,sentinel)
	}
	def apply[T : Manifest] ( position : ReadVec2i, dimensions : ReadVec2i, sentinel : T ) : WriteableFiniteGrid2D[T] = {
		val allPo2 = isPo2(dimensions.x) && isPo2(dimensions.y)
		if ( allPo2 ) {
			val twoToTheNDims = Vec2i(Po2Above(dimensions.x),Po2Above(dimensions.y))
			new Po2FiniteGrid2D[T]( VoxelCoord(position.x,position.y,0), twoToTheNDims, dimensions, manifest[T].newArray(dimensions.x * dimensions.y), sentinel )
		} else {
			new SimpleFiniteGrid2D[T]( VoxelCoord(position.x,position.y,0), dimensions,manifest[T].newArray(dimensions.x * dimensions.y), sentinel)
		}
	}
	def apply[T] (f : (Int,Int) => T, width : Int, height : Int) = {
		new FunctionBackedGrid[T](Vec2i(width,height),f)
	}
	def apply[T] (f : (Int,Int) => T, pos : ReadVec2i, width : Int, height : Int) = {
		new PositionedFunctionBackedGrid[T](pos, Vec2i(width,height),f)
	}
}
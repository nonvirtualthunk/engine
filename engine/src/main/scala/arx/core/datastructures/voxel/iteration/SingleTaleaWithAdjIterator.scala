package arx.core.datastructures.voxel.iteration

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/28/16
  * Time: 12:19 PM
  */

import arx.Prelude._
import arx.core.datastructures.voxel.Talea
import arx.core.datastructures.voxel.VoxelGrid
import arx.core.introspection.ReflectionAssistant
import arx.core.vec.coordinates.VoxelCoord
import scalaxy.loops._
import arx.core.vec._

protected[datastructures] class SingleTaleaWithAdjIterator[@specialized(Byte, Short, Int) T](grid: VoxelGrid[T], pos : VoxelCoord) {
	var compType = grid.dummyTalea.getComponentType
	protected final var prevYRow = ReflectionAssistant.arrayOf(compType, 34).asInstanceOf[Array[T]]
	protected final var curYRow = ReflectionAssistant.arrayOf(compType, 34).asInstanceOf[Array[T]]
	protected final var nextYRow = ReflectionAssistant.arrayOf(compType, 34).asInstanceOf[Array[T]]
	protected final val underRow = ReflectionAssistant.arrayOf(compType, 32).asInstanceOf[Array[T]]
	protected final val overRow = ReflectionAssistant.arrayOf(compType, 32).asInstanceOf[Array[T]]

	var _tx = pos.x
	var _ty = pos.y
	var _tz = pos.z
	def tx = _tx
	def ty = _ty
	def tz = _tz

	var td = Talea.dimension

	var dx = -1
	var dy = -1
	var dz = -1

	def moveTo(x : Int, y : Int, z : Int): Unit = {
		if (dz != z) {
			loadRowLong(tx, ty - 1, tz + z, grid, prevYRow)
			loadRowLong(tx, ty, tz + z, grid, curYRow)
		}
		if (dy != y) {
			if (y != 0) {
				val tmp = prevYRow
				prevYRow = curYRow
				curYRow = nextYRow
				nextYRow = tmp
			}

			loadRowLong(tx, ty + y + 1, tz + z, grid, nextYRow)
			loadRow(tx, ty + y, tz + z - 1, grid, underRow)
			loadRow(tx, ty + y, tz + z + 1, grid, overRow)
		}
		dx = x
		dy = y
		dz = z
	}

	@inline def left = { curYRow(dx) }
	@inline def center = { curYRow(dx+1) }
	@inline def right = { curYRow(dx+2) }
	@inline def up = { overRow(dx) }
	@inline def down = { underRow(dx) }
	@inline def front = { nextYRow(dx+1) }
	@inline def back = { prevYRow(dx+1) }
	@inline def adj(i : Int) = {
		if (i == 0){ curYRow(dx) }
		else if (i == 1){ prevYRow(dx+1) }
		else if (i == 2){ underRow(dx) }
		else if (i == 3){ curYRow(dx+2) }
		else if (i == 4){ nextYRow(dx+1) }
		else if (i == 5){ overRow(dx) }
		else if (i == 6){ curYRow(dx+1) }
		else { curYRow(0) }
	}

	private[this] final def loadRow(sx: Int, sy: Int, sz: Int, grid: VoxelGrid[T], arr: Array[T]): Unit = {
		val talea = grid.grid.getOrElse(sx,sy,sz, grid.dummyTalea)
		val ty = sy - talea.y
		val tz = sz - talea.z
		talea.loadRow(0,ty,tz,Talea.dimension, 0, arr)
	}

	private[this] final def loadRowLong(sx: Int, sy: Int, sz: Int, grid: VoxelGrid[T], arr: Array[T]): Unit = {
		val talea = grid.grid.getOrElse(sx,sy,sz, grid.dummyTalea)
		val ty = sy - talea.y
		val tz = sz - talea.z
		talea.loadRow(0,ty,tz,32, 1, arr)
		arr(0) = grid(sx-1,sy,sz)
		arr(33) = grid(sx+Talea.dimension,sy,sz)
	}
}
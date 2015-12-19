package arx.core.datastructures.traits

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/6/15
 * Time: 8:45 AM
 */

import arx.Prelude._
import arx.core.vec.ReadVec3i
import scalaxy.loops._

trait TVoxelView[@specialized(Byte,Short,Int) T] {
	def apply (x: Int,y : Int,z: Int): T
}

trait TUpdatableVoxelView[@specialized(Byte,Short,Int) T] extends TVoxelView[T] {
	def update (x: Int,y : Int,z: Int,t: T)
}

trait TVoxelStore[@specialized(Byte,Short,Int) T] extends TUpdatableVoxelView[T] {}

trait TInfiniteVoxelView[@specialized(Byte,Short,Int) T] extends TVoxelView[T]{
	def apply(v : ReadVec3i): T = this.apply(v.x,v.y,v.z)
}
trait TInfiniteVoxelStore[@specialized(Byte,Short,Int) T] extends TInfiniteVoxelView[T] with TVoxelStore[T]{
	def update(v:ReadVec3i,b: T){ this.update(v.x,v.y,v.z,b) }
}
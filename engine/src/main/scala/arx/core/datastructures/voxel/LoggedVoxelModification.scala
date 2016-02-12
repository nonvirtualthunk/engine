package arx.core.datastructures.voxel

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/7/16
  * Time: 4:39 PM
  */

import arx.Prelude._
import scalaxy.loops._
import arx.core.vec._

object LoggedVoxelModification {
	def apply[T] (v : ReadVec3i, ov : T, nv : T, rev : Int) = {
		new LoggedVoxelModification[T](v,ov,nv,rev)
	}
	def apply[T] (x : Int, y : Int, z : Int, ov : T, nv : T, rev : Int) = {
		new LoggedVoxelModification[T](x.toByte,y.toByte,z.toByte,ov,nv,rev)
	}
}

class LoggedVoxelModification[@specialized(Byte,Short,Int) T](val localX : Byte, val localY : Byte, val localZ : Byte, val oldValue : T, val newValue : T, val revision : Int ) {
	def this (v : ReadVec3i, ov : T, nv : T, rev : Int) {
		this(v.x.toByte,v.y.toByte,v.z.toByte,ov,nv,rev)
	}
	def localPosition = ReadVec3i(localX,localY,localZ)
}
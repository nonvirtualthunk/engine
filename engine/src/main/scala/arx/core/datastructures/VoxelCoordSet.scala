package arx.core.datastructures

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/14/13
 * Time: 11:35 AM
 */

import arx.Prelude._
import scalaxy.loops._
import arx.core.vec.coordinates._
import com.carrotsearch.hppc.{IntOpenHashSet, LongOpenHashSet}

class VoxelCoordSet extends Traversable[VoxelCoord] {
	val intern = new LongOpenHashSet()

	def foreach[U](f: (VoxelCoord) => U): Unit = {
		val iter = intern.iterator()
		val vc = new MutableVoxelCoord()
		while ( iter.hasNext ) {
			val L = iter.next().value
			LtoVC(L,vc)
			f(vc)
		}
	}

	def contains ( x:Int,y:Int,z:Int ) { intern.contains( VCtoL(x,y,z) ) }
	def contains ( v : VoxelCoord ) { intern.contains( VCtoL(v) ) }
	
	def add ( x:Int,y:Int,z:Int ) { intern.add( VCtoL(x,y,z) ) }
	def remove ( x:Int,y:Int,z:Int ) { intern.remove( VCtoL(x,y,z) ) }

	def add ( v : VoxelCoord ) { intern.add( VCtoL(v) ) }
	def remove ( v : VoxelCoord ) { intern.remove( VCtoL(v) ) }

	@inline
	protected def VCtoL ( x : Int , y : Int , z : Int ) = {
		(z.toLong << 40L) + (y.toLong << 20L) + x.toLong
	}
	@inline
	protected def VCtoL ( vc : VoxelCoord ) = {
		(vc.z.toLong << 40L) + (vc.y.toLong << 20L) + vc.x.toLong
	}
	@inline
	protected def LtoVC ( l : Long , vc : MutableVoxelCoord ) = {
		val x = l & ((1 << 20L) - 1)
		val y = (l >> 20L) & ((1 << 20L) - 1)
		val z = l >> 40L
		vc.x = x.toInt
		vc.y = y.toInt
		vc.z = z.toInt
	}
}

class VoxelCoordSet2D extends Traversable[VoxelCoord] {
	val intern = new IntOpenHashSet()

	def foreach[U](f: (VoxelCoord) => U): Unit = {
		val iter = intern.iterator()
		val vc = new MutableVoxelCoord()
		while ( iter.hasNext ) {
			val L = iter.next().value
			LtoVC(L,vc)
			f(vc)
		}
	}

	def contains ( x:Int,y:Int ) = { intern.contains( VCtoL(x,y) ) }
	def contains ( v : VoxelCoord ) = { intern.contains( VCtoL(v) ) }

	def add ( x:Int,y:Int ) { intern.add( VCtoL(x,y) ) }
	def remove ( x:Int,y:Int ) { intern.remove( VCtoL(x,y) ) }

	def add ( v : VoxelCoord ) { intern.add( VCtoL(v) ) }
	def remove ( v : VoxelCoord ) { intern.remove( VCtoL(v) ) }

	@inline
	protected def VCtoL ( x : Int , y : Int ) = {
		(y << 15) + x
	}
	@inline
	protected def VCtoL ( vc : VoxelCoord ) = {
		(vc.y << 15) + vc.x
	}
	@inline
	protected def LtoVC ( l : Int , vc : MutableVoxelCoord ) = {
		val x = l & ((1 << 15) - 1)
		val y = l >> 15
		vc.x = x
		vc.y = y
	}
}

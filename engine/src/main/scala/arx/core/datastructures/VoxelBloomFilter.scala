package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/10/12
 * Time: 4:18 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.vec.coordinates.VoxelCoord

class VoxelBloomFilter(expectedSize : Int,maximumFalsePositiveRate : Float) {
	val bloom = BloomFilter.withFalsePositiveProbability(expectedSize,maximumFalsePositiveRate)

	def hash ( x : Int ,y : Int , z : Int ) : Int = {
		((x&1023) << 20) + ((y&1023) << 10) + z
	}

	def contains ( x : Int , y : Int , z : Int ) : Boolean = {
		bloom.contains(hash(x,y,z))
	}
	def add ( x : Int , y : Int , z : Int ) { bloom.add(hash(x,y,z)) }
	def contains ( v : VoxelCoord ) : Boolean= contains(v.x,v.y,v.z)
	def add ( v : VoxelCoord ) { add(v.x,v.y,v.z) }
	def addIfAbsent ( x : Int , y : Int , z : Int ) = {
		val h = hash(x,y,z)
		if ( ! bloom.contains(h) ) {
			bloom.add(h)
			true
		} else { false }
	}

	def addByHash ( h : Int ) { bloom.add(h) }
	def containsHash ( h : Int ) = { bloom.contains(h) }
}
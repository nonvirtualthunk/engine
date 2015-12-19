package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/12
 * Time: 6:19 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import collection.{mutable, GenTraversableOnce}
import arx.core.vec.coordinates.VoxelCoord

class ProbabilisticHashSet[T](expectedSize : Int,falsePositiveRate : Float) extends TBareBonesSet[T] {
	val bloom = BloomFilter.withFalsePositiveProbability(expectedSize,falsePositiveRate)
	val list = new mutable.ArrayBuffer[T](expectedSize)

	if ( bloom.expectedFalsePositiveProbability > falsePositiveRate ) { Noto.info("Bloom filter had unexpectedly high FP rate : " + bloom.expectedFalsePositiveProbability ) }

	var incrementor = ProbabilisticHashSet.incrementor
	ProbabilisticHashSet.incrementor = (ProbabilisticHashSet.incrementor + 1) & 127

//	val innerSet = new mutable.HashSet[T]
//
//	def contains ( t : T ) = innerSet.contains(t)
//	def add ( t : T ) {
//		innerSet.add(t)
//	}
//	def addAll ( set : TBareBonesSet[T] ) {
//		for ( elem <- set ) {
//			innerSet.add(elem)
//		}
//	}
//	override def isEmpty = innerSet.isEmpty
//
//	def foreach[U](f: (T) => U) {
//		innerSet.foreach(f)
//	}

	def hash ( t : T ) = t.hashCode() + incrementor

	def contains ( t : T ) = bloom.contains(hash(t))

	def add ( t : T ) : Boolean = {
		val h = hash(t)
		if ( ! bloom.contains(h) ) {
			bloom.add(h)
			list.append(t)

			if ( list.size > expectedSize ) {
				Noto.info("Size has exceeded expectation : " + list.size + " > " + expectedSize)
			}
			true
		} else { false }
	}

	def addAll ( set : TBareBonesSet[T] ) {
		for ( elem <- set ) {
			add(elem)
		}
	}

	override def isEmpty = list.isEmpty

	override def values = list

	def foreach[U](f: (T) => U) {
		values.foreach(f)
	}

	override def size = list.size

	def addCertain(t: T) { list.append(t) }
}
object ProbabilisticHashSet {
	var incrementor = 0
}

trait TBareBonesSet[T] extends Traversable[T] {
	def contains ( t : T ) : Boolean
	def add ( t : T ) : Boolean
	def addCertain ( t : T )
	def addAll ( t : TBareBonesSet[T] )
	def values : Traversable[T] = this
}

class ProbabilisticVoxelSet(expectedSize : Int,falsePositiveRate : Float) extends TBareBonesSet[VoxelCoord] {
	val bloom = BloomFilter.withFalsePositiveProbability(expectedSize,falsePositiveRate)
	val list = new mutable.ArrayBuffer[VoxelCoord](expectedSize)
//	val list = new CoordinateBinaryVector(expectedSize)

	var checkSize = false

	if ( bloom.expectedFalsePositiveProbability > falsePositiveRate ) { Noto.info("Bloom filter had unexpectedly high FP rate : " + bloom.expectedFalsePositiveProbability ) }

//	val hashSet = if ( deterministic ) { new mutable.HashSet[VoxelCoord] } else { null }

	var xIncrementor = ProbabilisticHashSet.incrementor
	var yIncrementor = ProbabilisticHashSet.incrementor >> 1
	ProbabilisticHashSet.incrementor = (ProbabilisticHashSet.incrementor + 1) & 255

	def hash ( t : VoxelCoord ) : Int = {
//		hash( t.x + xIncrementor , t.y + yIncrementor , t.z )
		hash(t.x,t.y,t.z)
	}
	def hash ( x : Int ,y : Int , z : Int ) : Int = {
//		VoxelCoord.hash(x + xIncrementor, y + yIncrementor , z)
		((x&1023) << 20) + ((y&1023) << 10) + z
	}

	def contains ( t : VoxelCoord ) = bloom.contains(hash(t))
	def contains ( x : Int ,y : Int , z : Int ) = bloom.contains(hash(x,y,z))

	def addToBloomOnly ( x : Int , y : Int , z : Int ) { bloom.add(hash(x,y,z)) }

	def add ( x : Int , y : Int , z : Int ) {
		val h = hash(x,y,z)
		if ( ! bloom.contains(h) ) {
			bloom.add(h)
//			list.append(VoxelCoord(x,y,z))
			list.append(VoxelCoord(x,y,z))
			if ( checkSize && list.size > expectedSize ) {
				Noto.info("Size has exceeded expectation : " + list.size + " > " + expectedSize)
			}
			true
		} else { false }
	}
	def add ( t : VoxelCoord ) : Boolean = {
		val h = hash(t)
		if ( ! bloom.contains(h) ) {
			bloom.add(h)
			list.append(t)

//			hashSet.add(t)

			if ( checkSize && list.size > expectedSize ) {
				Noto.info("Size has exceeded expectation : " + list.size + " > " + expectedSize)
			}
			true
		} else { false }
//		else if ( ! hashSet.contains(t) ) { Noto.info("False positive : " + t ) }
	}

	def addAll ( set : TBareBonesSet[VoxelCoord] ) {
		for ( elem <- set ) {
			add(elem)
		}
	}

	override def isEmpty = list.isEmpty

	override def values = list

	def foreach[U](f: (VoxelCoord) => U) {
		values.foreach(f)
	}

	override def size = list.size

	def addCertain(t: VoxelCoord) {
		bloom.add(hash(t))
		list.append(t)
	}
	def addListOnly(t: VoxelCoord) { list.append(t) }
}
package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/8/12
 * Time: 2:41 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import math._
import annotation.tailrec

class BloomFilter(val size : Int, val expectedElements : Int ){
  require(size > 0)
  require(expectedElements > 0)

  val bitArray = new BitArray(size)
  val k = ceil((bitArray.size / expectedElements) * log(2.0)).toInt
  val expectedFalsePositiveProbability = pow(1 - exp(-k * 1.0 * expectedElements / bitArray.size), k)

  def add(hash : Int) {
    def add(i : Int, seed : Int) {
      if(i == k) return
      val next = xorRandom(seed)
      bitArray.set(next)
      add(i + 1, next)
    }
    add(0, hash)
  }

  def contains(hash : Int) : Boolean = {
	  @tailrec
    def contains(i : Int, seed : Int) : Boolean = {
      if(i == k) return true
      val next = xorRandom(seed)
      if (!bitArray.get(next)) return false
      contains(i + 1, next)
    }
    contains(0, hash)
  }

  private def xorRandom(i : Int) = {
    var y = i
    y ^= y << 13
    y ^= y >> 17
    y ^ y << 5
  }
}

class BloomFilterLong(val size : Int, val expectedElements : Int ){
  require(size > 0)
  require(expectedElements > 0)

  val bitArray = new BitArrayLong(size)
  val k = ceil((bitArray.size / expectedElements) * log(2.0)).toInt
  val expectedFalsePositiveProbability = pow(1 - exp(-k * 1.0 * expectedElements / bitArray.size), k)

  def add(hash : Long) {
    def add(i : Int, seed : Long) {
      if(i == k) return
      val next = xorRandom(seed)
      bitArray.set(next)
      add(i + 1, next)
    }
    add(0, hash)
  }

  def contains(hash : Long) : Boolean = {
	  @tailrec
    def contains(i : Int, seed : Long) : Boolean = {
      if(i == k) return true
      val next = xorRandom(seed)
      if (!bitArray.get(next)) return false
      contains(i + 1, next)
    }
    contains(0, hash)
  }

  private def xorRandom(i : Long) = {
    var y = i
    y ^= y << 13
    y ^= y >> 17
    y ^ y << 5
  }
}

object BloomFilter {

	def sizeFor(expectedElements: Int, falsePositiveRate: Float) = {
		var size = 32
		var found = false
		while ( ! found ) {
			val k = ceil((size / expectedElements) * log(2.0)).toInt
			val expectedFalsePositiveProbability = pow(1 - exp(-k * 1.0 * expectedElements / size), k)
			if ( expectedFalsePositiveProbability < falsePositiveRate ) {
				found = true
			} else {
				size *= 2
			}
		}
		size
	}

	def withFalsePositiveProbability ( expectedElements : Int , falsePositiveRate : Float ) = {
		val size = sizeFor(expectedElements,falsePositiveRate)
		new BloomFilter(size,expectedElements)
	}
	def withFalsePositiveProbabilityLong ( expectedElements : Int , falsePositiveRate : Float ) = {
		val size = sizeFor(expectedElements,falsePositiveRate)
		new BloomFilterLong(size,expectedElements)
	}
}
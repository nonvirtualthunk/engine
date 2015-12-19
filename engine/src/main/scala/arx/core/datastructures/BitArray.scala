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

class BitArray(bits : Int){
  val size = nextPow2(bits)
  require(isPowerOf2(size))
  private val data = new Array[Long](size >> 6)

  def set(index : Int) = data(idx(index)) |= (1L << index)
  def get(index : Int) = (data(idx(index)) & (1L << index)) != 0

  private val mask = size - 1
  private def idx(index : Int) = (index & mask) >> 6
  private def isPowerOf2(i : Int) = ((i - 1) & i) == 0
  private def nextPow2(i : Int) = {
    def highestBit(remainder : Int, c : Int) : Int =
       if(remainder > 0) highestBit(remainder >> 1, c + 1) else c
    require(i <= (1 << 30))
    val n = if(isPowerOf2(i)) i else 1 << highestBit(i, 0)
    assert(n >= i && i * 2 > n && isPowerOf2(n))
    n
  }
}

class BitArrayLong(bits : Int){
  val size = nextPow2(bits)
  require(isPowerOf2(size))
  private val data = new Array[Long](size >> 6)

  def set(index : Long) = data(idx(index)) |= (1L << index)
  def get(index : Long) = (data(idx(index)) & (1L << index)) != 0

  private val mask = size - 1
  private def idx(index : Long) = ((index & mask) >> 6).toInt
  private def isPowerOf2(i : Int) = ((i - 1) & i) == 0
  private def nextPow2(i : Int) = {
    def highestBit(remainder : Int, c : Int) : Int =
       if(remainder > 0) highestBit(remainder >> 1, c + 1) else c
    require(i <= (1 << 30))
    val n = if(isPowerOf2(i)) i else 1 << highestBit(i, 0)
    assert(n >= i && i * 2 > n && isPowerOf2(n))
    n
  }
}
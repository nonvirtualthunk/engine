package arx.core.math

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/6/15
 * Time: 8:48 AM
 */

import arx.Prelude._
import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec2i
import scalaxy.loops._

case class Rectf (var x : Float, var y : Float, var width : Float, var height : Float) {
	def maxX = x + w
	def minX = x
	def maxY = y + h
	def minY = y

	def w = width
	def h = height

	def min = ReadVec2f(minX,minY)
	def max = ReadVec2f(maxX,maxY)
}

case class Recti (var x : Int, var y : Int, var width : Int, var height : Int) {
	def maxX = x + w
	def minX = x
	def maxY = y + h
	def minY = y

	def w = width
	def h = height

	def min = ReadVec2i(minX,minY)
	def max = ReadVec2i(maxX,maxY)
}

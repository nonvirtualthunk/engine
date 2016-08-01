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

	def intersect (r2 : Rectf) : Rectf = {
		val r1 = this
		val lx = math.max(r1.x,r2.x)
		val ly = math.max(r1.y,r2.y)
		val hx = math.min(r1.x + r1.w,r2.x + r2.w)
		val hy = math.min(r1.y + r1.h,r2.y + r2.h)
		Rectf(lx,ly,hx-lx,hy-ly)
	}

	def toRecti : Recti = Recti(x.toInt,y.toInt,w.toInt,h.toInt)
}

object Rectf {
	def apply(r : Rectf) : Rectf = {
		Rectf(r.x,r.y,r.width,r.height)
	}
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

package arx.core.vec.coordinates

import arx.application.Noto
import arx.core.vec.{Vec2f, Vec3f}

import scala.language.implicitConversions

/**
  * Axial vec with an additional layer parameter indicating elevation
  */
case class AxialVec3(q : Int, r : Int, l : Int) {
	def asCubeVec = CubeVec(q, -q - r, r)

	def neighbor(n: Int) = this + AxialVec.AxialDelta(n)

	def neighbors = AxialVec.AxialDelta.map(d => this + d)

	def distance(other: AxialVec3) = {
		((q - other.q).abs + (q + r - other.q - other.r).abs + (r - other.r).abs) / 2.0f + (l - other.l).abs
	}

	def asCartesian: CartVec3 = CartVec3(q * 0.75f, (r + q * 0.5f) * 0.866025388f, l)
	def asCartesian(scale : Float) = Vec3f(q * 0.75f * scale, (r + q / 2.0f) * 0.866025388f * scale, l)

	def cartesianX(scale : Float = 1.0f) = q * 0.75f * scale
	def cartesianY(scale : Float = 1.0f) = (r + q * 0.5f) * 0.866025388f * scale

	def rounded = {
		val ax = CubeVec.roundedAxial(q, r).asAxialVec
		AxialVec3(ax.q, ax.r, l)
	}

	def sideClosestTo(other : AxialVec3, tieBreaker : AxialVec3) : Int = {
		qr.sideClosestTo(other.qr, tieBreaker.qr)
	}
	def sideClosestTo(other : AxialVec, tieBreaker : AxialVec) : Int = {
		qr.sideClosestTo(other, tieBreaker)
	}

	def +(other: AxialVec) = AxialVec3(q + other.q, r + other.r, l)
	def qr = AxialVec(q,r)
}

object AxialVec3 {
//	def apply(q : Int, r : Int, l : Int) : AxialVec3 = new AxialVec3(q,r,l)

	val Zero = AxialVec3(0,0,0)

	implicit def asAxialVec(v3 : AxialVec3) : AxialVec = v3.qr
}

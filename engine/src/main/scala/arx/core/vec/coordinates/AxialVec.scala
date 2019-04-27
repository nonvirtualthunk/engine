package arx.core.vec.coordinates

import arx.Prelude
import arx.application.Noto
import arx.core.vec.{Vec2f, Vec3i}

case class AxialVec(q: Int, r: Int) {
	def asCubeVec = CubeVec(q, -q - r, r)

	def neighbor(n: Int) = this + AxialVec.AxialDelta(n)

	def neighbors = AxialVec.AxialDelta.map(d => this + d)

	def distance(other: AxialVec) = {
		((q - other.q).abs + (q + r - other.q - other.r).abs + (r - other.r).abs) / 2.0f
	}

	def asCartesian: CartVec = CartVec(q * 0.75f, (r + q * 0.5f) * 0.866025388f)
	def asCartesian(scale : Float) = Vec2f(q * 0.75f * scale, (r + q / 2.0f) * 0.866025388f * scale)

	def cartesianX(scale : Float = 1.0f) = q * 0.75f * scale
	def cartesianY(scale : Float = 1.0f) = (r + q * 0.5f) * 0.866025388f * scale

	def rounded = CubeVec.roundedAxial(q, r).asAxialVec

	def sideClosestTo(other : AxialVec, tieBreaker : AxialVec) : Int = {
		if (other == this) {
			Noto.warn("Side closest to self doesn't really make sense")
			return 0
		}
		val selfCube = this.asCubeVec
		val deltaA = other.asCubeVec - selfCube
		val deltaB = tieBreaker.asCubeVec - selfCube
		val delta = deltaA + deltaB * 0.01f
		val a = delta.x - delta.y
		val b = delta.y - delta.z
		val c = delta.z - delta.x

		if (a.abs > b.abs && a.abs > c.abs) {
			if (a < 0.0f) {
				3
			} else {
				0
			}
		} else if (b.abs > a.abs && b.abs > c.abs) {
			if (b < 0.0f) {
				5
			} else {
				2
			}
		} else {
			if (c < 0.0) {
				1
			} else {
				4
			}
		}
	}

	def +(other: AxialVec) = AxialVec(q + other.q, r + other.r)

	def withLayer(l : Int) = AxialVec3(q,r,l)
}

object AxialVec {
	val Zero: AxialVec = AxialVec(0,0)

	val AxialDelta = Array(
		AxialVec(1, 0), AxialVec(1, -1), AxialVec(0, -1),
		AxialVec(-1, 0), AxialVec(-1, 1), AxialVec(0, 1))

	def fromCartesian(v : Vec2f, size : Float) : AxialVec = {
		val q = (v.x * 1.33333333f) / size
		val r = ((-v.x / 1.5f) + ((Prelude.sqrtf(3.0f)/1.5f) * v.y)) / size
		CubeVec.roundedAxial(q,r).asAxialVec
	}

	def fromCartesian(v : CartVec) : AxialVec = {
		val q = v.x * 1.33333333f
		val r = (-v.x / 1.5f) + ((Prelude.sqrtf(3.0f)/1.5f) * v.y)
		CubeVec.roundedAxial(q,r).asAxialVec
	}
}

class CubeVec(x_ : Int, y_ : Int, z_ : Int) extends Vec3i(x_, y_, z_) {
	def asAxialVec = AxialVec(x, z)

	def - (other : CubeVec) = CubeVec(x - other.x, y - other.y, z - other.z)
	def + (other : CubeVec) = CubeVec(x + other.x, y + other.y, z + other.z)
	override def * (f : Int) : CubeVec = CubeVec(x * f, y * f, z * f)
}

object CubeVec {
	val CubeDelta = Array(
		CubeVec(1,-1,0), CubeVec(1,0,-1), CubeVec(0,1,-1), CubeVec(-1,1,0), CubeVec(-1,0,1), CubeVec(0,-1,1)
	)

	def apply(x : Int, y : Int, z : Int) = new CubeVec(x,y,z)

	def roundedAxial(q: Float, r: Float): CubeVec = {
		val x = q
		val y = -q - r
		val z = r
		CubeVec.rounded(x, y, z)
	}

	def rounded(x: Float, y: Float, z: Float): CubeVec = {
		var rx = x.round
		var ry = y.round
		var rz = z.round

		val xDiff = (rx.toFloat - x).abs
		val yDiff = (ry.toFloat - y).abs
		val zDiff = (rz.toFloat - z).abs

		if (xDiff > yDiff && xDiff > zDiff) {
			rx = -ry - rz
		} else if (yDiff > zDiff) {
			ry = -rx - rz
		} else {
			rz = -rx - ry
		}
		CubeVec(rx, ry, rz)
	}
}

class HexDirection private (val q : Int) extends AnyVal
object HexDirection {
	val UpperRight = new HexDirection(0)
	val LowerRight = new HexDirection(1)
	val Bottom = new HexDirection(2)
	val LowerLeft = new HexDirection(3)
	val UpperLeft = new HexDirection(4)
	val Top = new HexDirection(5)
}
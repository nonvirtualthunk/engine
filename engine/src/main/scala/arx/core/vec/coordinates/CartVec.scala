package arx.core.vec.coordinates

import arx.core.vec.{ReadVec2f, ReadVec3f, Vec2f, Vec3f}

/**
  * Represents a hex-size agnostic cartesian coordinate
  */
class CartVec(x_ : Float, y_ : Float) extends Vec2f(x_, y_) {
	def +(v : CartVec) : CartVec = new CartVec(v.x + x,v.y + y)
	def -(v : CartVec) : CartVec = new CartVec(x - v.x, y - v.y)
	def *(v : CartVec) : CartVec = new CartVec(x * v.x, y * v.y)
	override def +(f : Float) : CartVec = new CartVec(x + f, y + f)
	override def -(f : Float) : CartVec = new CartVec(x - f, y - f)
	override def *(f : Float) : CartVec = new CartVec(x * f, y * f)

	def asAxialVec = AxialVec.fromCartesian(this)
}


object CartVec {
	def apply(x :Float, y : Float) = new CartVec(x,y)
	def apply(v : ReadVec2f) = new CartVec(v.x,v.y)

	val Zero = CartVec(0.0f,0.0f)
	val One = CartVec(1.0f,1.0f)
}



class CartVec3(x_ : Float, y_ : Float, z_ : Float) extends Vec3f(x_, y_, z_) {
	def +(v : CartVec3) : CartVec3 = new CartVec3(v.x + x,v.y + y,v.z + z)
	def -(v : CartVec3) : CartVec3 = new CartVec3(x - v.x,y - v.y,z - v.z)
	def *(v : CartVec3) : CartVec3 = new CartVec3(x * v.x, y * v.y, z * v.z)
	override def +(f : Float) : CartVec3 = new CartVec3(x + f, y + f, z + f)
	override def -(f : Float) : CartVec3 = new CartVec3(x - f, y - f, z - f)
	override def *(f : Float) : CartVec3 = new CartVec3(x * f, y * f,z * f)

	override def xy : CartVec = CartVec(x_, y_)

	def plusX(f : Float) : CartVec3 = new CartVec3(x + f,y,z)
	def plusY(f : Float) : CartVec3 = new CartVec3(x,y + f,z)
	def plusZ(f : Float) : CartVec3 = new CartVec3(x,y,z + f)

	def asAxialVec3 = {
		val qr = AxialVec.fromCartesian(this.xy, 1.0f)
		AxialVec3(qr.q, qr.r, this.z.toInt)
	}
	def asAxialVec = AxialVec.fromCartesian(this.xy, 1.0f)
}

object CartVec3 {
	def apply(x : Float, y : Float, z : Float) = new CartVec3(x,y,z)
	def apply(v : ReadVec3f) = new CartVec3(v.x,v.y,v.z)

	def Zero = new CartVec3(0.0f,0.0f,0.0f)
}
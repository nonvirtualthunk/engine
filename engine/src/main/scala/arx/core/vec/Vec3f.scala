package arx.core.vec;
@SerialVersionUID(9223372036854770000L)
class Vec3f extends ReadVec3f{
	def this(xa : Float,ya : Float,za : Float){ 
		this()
		xi = xa
		yi = ya
		zi = za
	}
	override def x_= ( s : Float ) { xi = s }
	override def y_= ( s : Float ) { yi = s }
	override def z_= ( s : Float ) { zi = s }
	override def r_= ( s : Float ) { xi = s }
	override def g_= ( s : Float ) { yi = s }
	override def b_= ( s : Float ) { zi = s }
	def +=(v : ReadVec3f) { xi += v.xi;yi += v.yi;zi += v.zi}
	def +=(s : Float) {xi += s;yi += s;zi += s}
	def -=(v : ReadVec3f) { xi -= v.xi;yi -= v.yi;zi -= v.zi}
	def -=(s : Float) {xi -= s;yi -= s;zi -= s}
	def *=(v : ReadVec3f) { xi *= v.xi;yi *= v.yi;zi *= v.zi}
	def *=(s : Float) {xi *= s;yi *= s;zi *= s}
	def /=(v : ReadVec3f) { xi /= v.xi;yi /= v.yi;zi /= v.zi}
	def /=(s : Float) {xi /= s;yi /= s;zi /= s}
	override def +(v : ReadVec3f) = new Vec3f(xi + v.xi,yi + v.yi,zi + v.zi)
	override def +(s : Float) = new Vec3f(xi + s,yi + s,zi + s)
	override def -(v : ReadVec3f) = new Vec3f(xi - v.xi,yi - v.yi,zi - v.zi)
	override def -(s : Float) = new Vec3f(xi - s,yi - s,zi - s)
	override def *(v : ReadVec3f) = new Vec3f(xi * v.xi,yi * v.yi,zi * v.zi)
	override def *(s : Float) = new Vec3f(xi * s,yi * s,zi * s)
	override def /(v : ReadVec3f) = new Vec3f(xi / v.xi,yi / v.yi,zi / v.zi)
	override def /(s : Float) = new Vec3f(xi / s,yi / s,zi / s)
	def update (i:Int,s:Float) { i match {
		case 0 => x = s
		case 1 => y = s
		case 2 => z = s
		case _ => 
	}}

}
object Vec3f{
	def apply (xa : Float,ya : Float,za : Float) = new Vec3f(xa : Float,ya : Float,za : Float)
	def apply (v : ReadVec3f) = new Vec3f(v.x,v.y,v.z)
	implicit def toWriteable (v : ReadVec3f) = new Vec3f(v.x,v.y,v.z)
	def apply (s : Float) = new Vec3f(s,s,s)
	val UnitX = new ReadVec3f(1,0,0)
	val UnitY = new ReadVec3f(0,1,0)
	val UnitZ = new ReadVec3f(0,0,1)
	object _Identity extends ReadVec3f(1,1,1) {
		override def * ( v : ReadVec3f ) = v
		def * ( v : Vec3f ) = v
	}
	val Identity : ReadVec3f = _Identity
	val One : ReadVec3f = _Identity
	object _Zero extends ReadVec3f{
		override def * ( v : ReadVec3f ) = this
		def * ( v : Vec3f ) = this
		override def / ( v : ReadVec3f ) = this
		def / ( v : Vec3f ) = this
		override def + ( v : ReadVec3f ) = v
		def + ( v : Vec3f ) = v
	}
	val Zero : ReadVec3f = _Zero
}

package arx.core.vec;
@SerialVersionUID(9223372036854770000L)
class Vec3i extends ReadVec3i{
	def this(xa : Int,ya : Int,za : Int){ 
		this()
		xi = xa
		yi = ya
		zi = za
	}
	override def x_= ( s : Int ) { xi = s }
	override def y_= ( s : Int ) { yi = s }
	override def z_= ( s : Int ) { zi = s }
	override def r_= ( s : Int ) { xi = s }
	override def g_= ( s : Int ) { yi = s }
	override def b_= ( s : Int ) { zi = s }
	def +=(v : ReadVec3i) { xi += v.xi;yi += v.yi;zi += v.zi}
	def +=(s : Int) {xi += s;yi += s;zi += s}
	def -=(v : ReadVec3i) { xi -= v.xi;yi -= v.yi;zi -= v.zi}
	def -=(s : Int) {xi -= s;yi -= s;zi -= s}
	def *=(v : ReadVec3i) { xi *= v.xi;yi *= v.yi;zi *= v.zi}
	def *=(s : Int) {xi *= s;yi *= s;zi *= s}
	def /=(v : ReadVec3i) { xi /= v.xi;yi /= v.yi;zi /= v.zi}
	def /=(s : Int) {xi /= s;yi /= s;zi /= s}
	def %=(v : ReadVec3i) { xi %= v.xi;yi %= v.yi;zi %= v.zi}
	def %=(s : Int) {xi %= s;yi %= s;zi %= s}
	def >>=(v : ReadVec3i) { xi >>= v.xi;yi >>= v.yi;zi >>= v.zi}
	def >>=(s : Int) {xi >>= s;yi >>= s;zi >>= s}
	def <<=(v : ReadVec3i) { xi <<= v.xi;yi <<= v.yi;zi <<= v.zi}
	def <<=(s : Int) {xi <<= s;yi <<= s;zi <<= s}
	override def +(v : ReadVec3i) = new Vec3i(xi + v.xi,yi + v.yi,zi + v.zi)
	override def +(s : Int) = new Vec3i(xi + s,yi + s,zi + s)
	override def -(v : ReadVec3i) = new Vec3i(xi - v.xi,yi - v.yi,zi - v.zi)
	override def -(s : Int) = new Vec3i(xi - s,yi - s,zi - s)
	override def *(v : ReadVec3i) = new Vec3i(xi * v.xi,yi * v.yi,zi * v.zi)
	override def *(s : Int) = new Vec3i(xi * s,yi * s,zi * s)
	override def /(v : ReadVec3i) = new Vec3i(xi / v.xi,yi / v.yi,zi / v.zi)
	override def /(s : Int) = new Vec3i(xi / s,yi / s,zi / s)
	override def %(v : ReadVec3i) = new Vec3i(xi % v.xi,yi % v.yi,zi % v.zi)
	override def %(s : Int) = new Vec3i(xi % s,yi % s,zi % s)
	override def >>(v : ReadVec3i) = new Vec3i(xi >> v.xi,yi >> v.yi,zi >> v.zi)
	override def >>(s : Int) = new Vec3i(xi >> s,yi >> s,zi >> s)
	override def <<(v : ReadVec3i) = new Vec3i(xi << v.xi,yi << v.yi,zi << v.zi)
	override def <<(s : Int) = new Vec3i(xi << s,yi << s,zi << s)
	def update (i:Int,s:Int) { i match {
		case 0 => x = s
		case 1 => y = s
		case 2 => z = s
		case _ => 
	}}

}
object Vec3i{
	def apply (xa : Int,ya : Int,za : Int) = new Vec3i(xa : Int,ya : Int,za : Int)
	def apply (v : ReadVec3i) = new Vec3i(v.x,v.y,v.z)
	def apply (v : ReadVec2i, z : Int) = new Vec3i(v.x,v.y,z)
	implicit def toWriteable (v : ReadVec3i) = new Vec3i(v.x,v.y,v.z)
	def apply (v : ReadVec3f) = new Vec3i(v.x.toInt,v.y.toInt,v.z.toInt)
	implicit def toFloatingPoint (v : Vec3i) = new Vec3f(v.x.toFloat,v.y.toFloat,v.z.toFloat)
	def apply (s : Int) = new Vec3i(s,s,s)
	val UnitX = new ReadVec3i(1,0,0)
	val UnitY = new ReadVec3i(0,1,0)
	val UnitZ = new ReadVec3i(0,0,1)
	object _Identity extends ReadVec3i(1,1,1) {
		override def * ( v : ReadVec3i ) = v
		def * ( v : Vec3i ) = v
	}
	val Identity : ReadVec3i = _Identity
	val One : ReadVec3i = _Identity
	object _Zero extends ReadVec3i{
		override def * ( v : ReadVec3i ) = this
		def * ( v : Vec3i ) = this
		override def / ( v : ReadVec3i ) = this
		def / ( v : Vec3i ) = this
		override def + ( v : ReadVec3i ) = v
		def + ( v : Vec3i ) = v
	}
	val Zero : ReadVec3i = _Zero
}

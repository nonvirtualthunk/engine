package arx.core.vec;
@SerialVersionUID(9223372036854770000L)
class Vec2i extends ReadVec2i{
	def this(xa : Int,ya : Int){ 
		this()
		xi = xa
		yi = ya
	}
	override def x_= ( s : Int ) { xi = s }
	override def y_= ( s : Int ) { yi = s }
	def +=(v : ReadVec2i) { xi += v.xi;yi += v.yi}
	def +=(s : Int) {xi += s;yi += s}
	def -=(v : ReadVec2i) { xi -= v.xi;yi -= v.yi}
	def -=(s : Int) {xi -= s;yi -= s}
	def *=(v : ReadVec2i) { xi *= v.xi;yi *= v.yi}
	def *=(s : Int) {xi *= s;yi *= s}
	def /=(v : ReadVec2i) { xi /= v.xi;yi /= v.yi}
	def /=(s : Int) {xi /= s;yi /= s}
	def %=(v : ReadVec2i) { xi %= v.xi;yi %= v.yi}
	def %=(s : Int) {xi %= s;yi %= s}
	def >>=(v : ReadVec2i) { xi >>= v.xi;yi >>= v.yi}
	def >>=(s : Int) {xi >>= s;yi >>= s}
	def <<=(v : ReadVec2i) { xi <<= v.xi;yi <<= v.yi}
	def <<=(s : Int) {xi <<= s;yi <<= s}
	override def +(v : ReadVec2i) = new Vec2i(xi + v.xi,yi + v.yi)
	override def +(s : Int) = new Vec2i(xi + s,yi + s)
	override def -(v : ReadVec2i) = new Vec2i(xi - v.xi,yi - v.yi)
	override def -(s : Int) = new Vec2i(xi - s,yi - s)
	override def *(v : ReadVec2i) = new Vec2i(xi * v.xi,yi * v.yi)
	override def *(s : Int) = new Vec2i(xi * s,yi * s)
	override def /(v : ReadVec2i) = new Vec2i(xi / v.xi,yi / v.yi)
	override def /(s : Int) = new Vec2i(xi / s,yi / s)
	override def %(v : ReadVec2i) = new Vec2i(xi % v.xi,yi % v.yi)
	override def %(s : Int) = new Vec2i(xi % s,yi % s)
	override def >>(v : ReadVec2i) = new Vec2i(xi >> v.xi,yi >> v.yi)
	override def >>(s : Int) = new Vec2i(xi >> s,yi >> s)
	override def <<(v : ReadVec2i) = new Vec2i(xi << v.xi,yi << v.yi)
	override def <<(s : Int) = new Vec2i(xi << s,yi << s)
	def update (i:Int,s:Int) { i match {
		case 0 => x = s
		case 1 => y = s
		case _ => 
	}}

}
object Vec2i{
	def apply (xa : Int,ya : Int) = new Vec2i(xa : Int,ya : Int)
	def apply (v : ReadVec2i) = new Vec2i(v.x,v.y)
	implicit def toWriteable (v : ReadVec2i) = new Vec2i(v.x,v.y)
	def apply (v : ReadVec2f) = new Vec2i(v.x.toInt,v.y.toInt)
	implicit def toFloatingPoint (v : Vec2i) = new Vec2f(v.x.toFloat,v.y.toFloat)
	def apply (s : Int) = new Vec2i(s,s)
	val UnitX = new ReadVec2i(1,0)
	val UnitY = new ReadVec2i(0,1)
	object _Identity extends ReadVec2i(1,1) {
		override def * ( v : ReadVec2i ) = v
		def * ( v : Vec2i ) = v
	}
	val Identity : ReadVec2i = _Identity
	val One : ReadVec2i = _Identity
	object _Zero extends ReadVec2i{
		override def * ( v : ReadVec2i ) = this
		def * ( v : Vec2i ) = this
		override def / ( v : ReadVec2i ) = this
		def / ( v : Vec2i ) = this
		override def + ( v : ReadVec2i ) = v
		def + ( v : Vec2i ) = v
	}
	val Zero : ReadVec2i = _Zero
}

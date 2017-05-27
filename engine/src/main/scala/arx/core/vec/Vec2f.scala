package arx.core.vec;
@SerialVersionUID(9223372036854770000L)
class Vec2f extends ReadVec2f{
	def this(xa : Float,ya : Float){ 
		this()
		xi = xa
		yi = ya
	}
	override def x_= ( s : Float ) { xi = s }
	override def y_= ( s : Float ) { yi = s }
	def +=(v : ReadVec2f) { xi += v.xi;yi += v.yi}
	def +=(s : Float) {xi += s;yi += s}
	def -=(v : ReadVec2f) { xi -= v.xi;yi -= v.yi}
	def -=(s : Float) {xi -= s;yi -= s}
	def *=(v : ReadVec2f) { xi *= v.xi;yi *= v.yi}
	def *=(s : Float) {xi *= s;yi *= s}
	def /=(v : ReadVec2f) { xi /= v.xi;yi /= v.yi}
	def /=(s : Float) {xi /= s;yi /= s}
	override def +(v : ReadVec2f) = new Vec2f(xi + v.xi,yi + v.yi)
	override def +(s : Float) = new Vec2f(xi + s,yi + s)
	override def -(v : ReadVec2f) = new Vec2f(xi - v.xi,yi - v.yi)
	override def -(s : Float) = new Vec2f(xi - s,yi - s)
	override def *(v : ReadVec2f) = new Vec2f(xi * v.xi,yi * v.yi)
	override def *(s : Float) = new Vec2f(xi * s,yi * s)
	override def /(v : ReadVec2f) = new Vec2f(xi / v.xi,yi / v.yi)
	override def /(s : Float) = new Vec2f(xi / s,yi / s)
	def update (i:Int,s:Float) { i match {
		case 0 => x = s
		case 1 => y = s
		case _ => 
	}}

}
object Vec2f{
	def apply (xa : Float,ya : Float) = new Vec2f(xa : Float,ya : Float)
	def apply (v : ReadVec2f) = new Vec2f(v.x,v.y)
	def apply (v : ReadVec2i) = new Vec2f(v.x,v.y)
	implicit def toWriteable (v : ReadVec2f) = new Vec2f(v.x,v.y)
	def apply (s : Float) = new Vec2f(s,s)
	val UnitX = new ReadVec2f(1,0)
	val UnitY = new ReadVec2f(0,1)
	object _Identity extends ReadVec2f(1,1) {
		override def * ( v : ReadVec2f ) = v
		def * ( v : Vec2f ) = v
	}
	val Identity : ReadVec2f = _Identity
	val One : ReadVec2f = _Identity
	object _Zero extends ReadVec2f{
		override def * ( v : ReadVec2f ) = this
		def * ( v : Vec2f ) = this
		override def / ( v : ReadVec2f ) = this
		def / ( v : Vec2f ) = this
		override def + ( v : ReadVec2f ) = v
		def + ( v : Vec2f ) = v
	}
	val Zero : ReadVec2f = _Zero
}

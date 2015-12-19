package arx.core.vec;
@SerialVersionUID(9223372036854770000L)
class Vec4f extends ReadVec4f{
	def this(ra : Float,ga : Float,ba : Float,aa : Float){ 
		this()
		ri = ra
		gi = ga
		bi = ba
		ai = aa
	}
	override def r_= ( s : Float ) { ri = s }
	override def g_= ( s : Float ) { gi = s }
	override def b_= ( s : Float ) { bi = s }
	override def a_= ( s : Float ) { ai = s }
	def +=(v : ReadVec4f) { ri += v.ri;gi += v.gi;bi += v.bi;ai += v.ai}
	def +=(s : Float) {ri += s;gi += s;bi += s;ai += s}
	def -=(v : ReadVec4f) { ri -= v.ri;gi -= v.gi;bi -= v.bi;ai -= v.ai}
	def -=(s : Float) {ri -= s;gi -= s;bi -= s;ai -= s}
	def *=(v : ReadVec4f) { ri *= v.ri;gi *= v.gi;bi *= v.bi;ai *= v.ai}
	def *=(s : Float) {ri *= s;gi *= s;bi *= s;ai *= s}
	def /=(v : ReadVec4f) { ri /= v.ri;gi /= v.gi;bi /= v.bi;ai /= v.ai}
	def /=(s : Float) {ri /= s;gi /= s;bi /= s;ai /= s}
	override def +(v : ReadVec4f) = new Vec4f(ri + v.ri,gi + v.gi,bi + v.bi,ai + v.ai)
	override def +(s : Float) = new Vec4f(ri + s,gi + s,bi + s,ai + s)
	override def -(v : ReadVec4f) = new Vec4f(ri - v.ri,gi - v.gi,bi - v.bi,ai - v.ai)
	override def -(s : Float) = new Vec4f(ri - s,gi - s,bi - s,ai - s)
	override def *(v : ReadVec4f) = new Vec4f(ri * v.ri,gi * v.gi,bi * v.bi,ai * v.ai)
	override def *(s : Float) = new Vec4f(ri * s,gi * s,bi * s,ai * s)
	override def /(v : ReadVec4f) = new Vec4f(ri / v.ri,gi / v.gi,bi / v.bi,ai / v.ai)
	override def /(s : Float) = new Vec4f(ri / s,gi / s,bi / s,ai / s)
	def update (i:Int,s:Float) { i match {
		case 0 => r = s
		case 1 => g = s
		case 2 => b = s
		case 3 => a = s
		case _ => 
	}}

}
object Vec4f{
	def apply (ra : Float,ga : Float,ba : Float,aa : Float) = new Vec4f(ra : Float,ga : Float,ba : Float,aa : Float)
	def apply (v : ReadVec4f) = new Vec4f(v.r,v.g,v.b,v.a)
	implicit def toWriteable (v : ReadVec4f) = new Vec4f(v.r,v.g,v.b,v.a)
	def apply (s : Float) = new Vec4f(s,s,s,s)
	def apply (rgb : Float, a : Float) = new Vec4f(rgb,rgb,rgb,a)
	val UnitR = new ReadVec4f(1,0,0,0)
	val UnitG = new ReadVec4f(0,1,0,0)
	val UnitB = new ReadVec4f(0,0,1,0)
	val UnitA = new ReadVec4f(0,0,0,1)
	object _Identity extends ReadVec4f(1,1,1,1) {
		override def * ( v : ReadVec4f ) = v
		def * ( v : Vec4f ) = v
	}
	val Identity : ReadVec4f = _Identity
	val One : ReadVec4f = _Identity
	object _Zero extends ReadVec4f{
		override def * ( v : ReadVec4f ) = this
		def * ( v : Vec4f ) = this
		override def / ( v : ReadVec4f ) = this
		def / ( v : Vec4f ) = this
		override def + ( v : ReadVec4f ) = v
		def + ( v : Vec4f ) = v
	}
	val Zero : ReadVec4f = _Zero
}

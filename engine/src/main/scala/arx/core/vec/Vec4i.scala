package arx.core.vec;
@SerialVersionUID(9223372036854770000L)
class Vec4i extends ReadVec4i{
	def this(ra : Int,ga : Int,ba : Int,aa : Int){ 
		this()
		ri = ra
		gi = ga
		bi = ba
		ai = aa
	}
	override def r_= ( s : Int ) { ri = s }
	override def g_= ( s : Int ) { gi = s }
	override def b_= ( s : Int ) { bi = s }
	override def a_= ( s : Int ) { ai = s }
	def +=(v : ReadVec4i) { ri += v.ri;gi += v.gi;bi += v.bi;ai += v.ai}
	def +=(s : Int) {ri += s;gi += s;bi += s;ai += s}
	def -=(v : ReadVec4i) { ri -= v.ri;gi -= v.gi;bi -= v.bi;ai -= v.ai}
	def -=(s : Int) {ri -= s;gi -= s;bi -= s;ai -= s}
	def *=(v : ReadVec4i) { ri *= v.ri;gi *= v.gi;bi *= v.bi;ai *= v.ai}
	def *=(s : Int) {ri *= s;gi *= s;bi *= s;ai *= s}
	def /=(v : ReadVec4i) { ri /= v.ri;gi /= v.gi;bi /= v.bi;ai /= v.ai}
	def /=(s : Int) {ri /= s;gi /= s;bi /= s;ai /= s}
	def %=(v : ReadVec4i) { ri %= v.ri;gi %= v.gi;bi %= v.bi;ai %= v.ai}
	def %=(s : Int) {ri %= s;gi %= s;bi %= s;ai %= s}
	def >>=(v : ReadVec4i) { ri >>= v.ri;gi >>= v.gi;bi >>= v.bi;ai >>= v.ai}
	def >>=(s : Int) {ri >>= s;gi >>= s;bi >>= s;ai >>= s}
	def <<=(v : ReadVec4i) { ri <<= v.ri;gi <<= v.gi;bi <<= v.bi;ai <<= v.ai}
	def <<=(s : Int) {ri <<= s;gi <<= s;bi <<= s;ai <<= s}
	override def +(v : ReadVec4i) = new Vec4i(ri + v.ri,gi + v.gi,bi + v.bi,ai + v.ai)
	override def +(s : Int) = new Vec4i(ri + s,gi + s,bi + s,ai + s)
	override def -(v : ReadVec4i) = new Vec4i(ri - v.ri,gi - v.gi,bi - v.bi,ai - v.ai)
	override def -(s : Int) = new Vec4i(ri - s,gi - s,bi - s,ai - s)
	override def *(v : ReadVec4i) = new Vec4i(ri * v.ri,gi * v.gi,bi * v.bi,ai * v.ai)
	override def *(s : Int) = new Vec4i(ri * s,gi * s,bi * s,ai * s)
	override def /(v : ReadVec4i) = new Vec4i(ri / v.ri,gi / v.gi,bi / v.bi,ai / v.ai)
	override def /(s : Int) = new Vec4i(ri / s,gi / s,bi / s,ai / s)
	override def %(v : ReadVec4i) = new Vec4i(ri % v.ri,gi % v.gi,bi % v.bi,ai % v.ai)
	override def %(s : Int) = new Vec4i(ri % s,gi % s,bi % s,ai % s)
	override def >>(v : ReadVec4i) = new Vec4i(ri >> v.ri,gi >> v.gi,bi >> v.bi,ai >> v.ai)
	override def >>(s : Int) = new Vec4i(ri >> s,gi >> s,bi >> s,ai >> s)
	override def <<(v : ReadVec4i) = new Vec4i(ri << v.ri,gi << v.gi,bi << v.bi,ai << v.ai)
	override def <<(s : Int) = new Vec4i(ri << s,gi << s,bi << s,ai << s)
	def update (i:Int,s:Int) { i match {
		case 0 => r = s
		case 1 => g = s
		case 2 => b = s
		case 3 => a = s
		case _ => 
	}}

}
object Vec4i{
	def apply (ra : Int,ga : Int,ba : Int,aa : Int) = new Vec4i(ra : Int,ga : Int,ba : Int,aa : Int)
	def apply (v : ReadVec4i) = new Vec4i(v.r,v.g,v.b,v.a)
	implicit def toWriteable (v : ReadVec4i) = new Vec4i(v.r,v.g,v.b,v.a)
	def apply (v : ReadVec4f) = new Vec4i(v.r.toInt,v.g.toInt,v.b.toInt,v.a.toInt)
	implicit def toFloatingPoint (v : Vec4i) = new Vec4f(v.r.toFloat,v.g.toFloat,v.b.toFloat,v.a.toFloat)
	def apply (s : Int) = new Vec4i(s,s,s,s)
	def apply (s : Int,a:Int) = new Vec4i(s,s,s,a)
	val UnitR = new ReadVec4i(1,0,0,0)
	val UnitG = new ReadVec4i(0,1,0,0)
	val UnitB = new ReadVec4i(0,0,1,0)
	val UnitA = new ReadVec4i(0,0,0,1)
	object _Identity extends ReadVec4i(1,1,1,1) {
		override def * ( v : ReadVec4i ) = v
		def * ( v : Vec4i ) = v
	}
	val Identity : ReadVec4i = _Identity
	val One : ReadVec4i = _Identity
	object _Zero extends ReadVec4i{
		override def * ( v : ReadVec4i ) = this
		def * ( v : Vec4i ) = this
		override def / ( v : ReadVec4i ) = this
		def / ( v : Vec4i ) = this
		override def + ( v : ReadVec4i ) = v
		def + ( v : Vec4i ) = v
	}
	val Zero : ReadVec4i = _Zero
}

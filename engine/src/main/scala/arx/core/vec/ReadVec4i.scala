package arx.core.vec;
import arx.core.Moddable
import arx.core.SelfModdable
@SerialVersionUID(9223372036854770000L)
class ReadVec4i extends InternVec4i with SelfModdable[ReadVec4i]{
	def this(ra : Int,ga : Int,ba : Int,aa : Int){ 
		this()
		ri = ra
		gi = ga
		bi = ba
		ai = aa
	}
	def +(m : Moddable[ReadVec4i]) = { val v = m.resolve(); new ReadVec4i(ri + v.ri,gi + v.gi,bi + v.bi,ai + v.ai) }
	def +(v : ReadVec4i) = new ReadVec4i(ri + v.ri,gi + v.gi,bi + v.bi,ai + v.ai)
	def +(s : Int) = new ReadVec4i(ri + s,gi + s,bi + s,ai + s)
	def -(m : Moddable[ReadVec4i]) = { val v = m.resolve(); new ReadVec4i(ri - v.ri,gi - v.gi,bi - v.bi,ai - v.ai) }
	def -(v : ReadVec4i) = new ReadVec4i(ri - v.ri,gi - v.gi,bi - v.bi,ai - v.ai)
	def -(s : Int) = new ReadVec4i(ri - s,gi - s,bi - s,ai - s)
	def *(m : Moddable[ReadVec4i]) = { val v = m.resolve(); new ReadVec4i(ri * v.ri,gi * v.gi,bi * v.bi,ai * v.ai) }
	def *(v : ReadVec4i) = new ReadVec4i(ri * v.ri,gi * v.gi,bi * v.bi,ai * v.ai)
	def *(s : Int) = new ReadVec4i(ri * s,gi * s,bi * s,ai * s)
	def /(m : Moddable[ReadVec4i]) = { val v = m.resolve(); new ReadVec4i(ri / v.ri,gi / v.gi,bi / v.bi,ai / v.ai) }
	def /(v : ReadVec4i) = new ReadVec4i(ri / v.ri,gi / v.gi,bi / v.bi,ai / v.ai)
	def /(s : Int) = new ReadVec4i(ri / s,gi / s,bi / s,ai / s)
	def %(m : Moddable[ReadVec4i]) = { val v = m.resolve(); new ReadVec4i(ri % v.ri,gi % v.gi,bi % v.bi,ai % v.ai) }
	def %(v : ReadVec4i) = new ReadVec4i(ri % v.ri,gi % v.gi,bi % v.bi,ai % v.ai)
	def %(s : Int) = new ReadVec4i(ri % s,gi % s,bi % s,ai % s)
	def >>(m : Moddable[ReadVec4i]) = { val v = m.resolve(); new ReadVec4i(ri >> v.ri,gi >> v.gi,bi >> v.bi,ai >> v.ai) }
	def >>(v : ReadVec4i) = new ReadVec4i(ri >> v.ri,gi >> v.gi,bi >> v.bi,ai >> v.ai)
	def >>(s : Int) = new ReadVec4i(ri >> s,gi >> s,bi >> s,ai >> s)
	def <<(m : Moddable[ReadVec4i]) = { val v = m.resolve(); new ReadVec4i(ri << v.ri,gi << v.gi,bi << v.bi,ai << v.ai) }
	def <<(v : ReadVec4i) = new ReadVec4i(ri << v.ri,gi << v.gi,bi << v.bi,ai << v.ai)
	def <<(s : Int) = new ReadVec4i(ri << s,gi << s,bi << s,ai << s)
	def r = ri
	protected def r_= ( s : Int ) { ri = s }
	def g = gi
	protected def g_= ( s : Int ) { gi = s }
	def b = bi
	protected def b_= ( s : Int ) { bi = s }
	def a = ai
	protected def a_= ( s : Int ) { ai = s }
	def rg = new Vec2i(ri,gi)
	def gr = new Vec2i(gi,ri)
	def rb = new Vec2i(ri,bi)
	def br = new Vec2i(bi,ri)
	def ra = new Vec2i(ri,ai)
	def ar = new Vec2i(ai,ri)
	def gb = new Vec2i(gi,bi)
	def bg = new Vec2i(bi,gi)
	def ga = new Vec2i(gi,ai)
	def ag = new Vec2i(ai,gi)
	def ba = new Vec2i(bi,ai)
	def ab = new Vec2i(ai,bi)
	def rgb = new Vec3i(ri,gi,bi)
	def rbg = new Vec3i(ri,bi,gi)
	def grb = new Vec3i(gi,ri,bi)
	def gbr = new Vec3i(gi,bi,ri)
	def brg = new Vec3i(bi,ri,gi)
	def bgr = new Vec3i(bi,gi,ri)
	def rga = new Vec3i(ri,gi,ai)
	def rag = new Vec3i(ri,ai,gi)
	def gra = new Vec3i(gi,ri,ai)
	def gar = new Vec3i(gi,ai,ri)
	def arg = new Vec3i(ai,ri,gi)
	def agr = new Vec3i(ai,gi,ri)
	def rba = new Vec3i(ri,bi,ai)
	def rab = new Vec3i(ri,ai,bi)
	def bra = new Vec3i(bi,ri,ai)
	def bar = new Vec3i(bi,ai,ri)
	def arb = new Vec3i(ai,ri,bi)
	def abr = new Vec3i(ai,bi,ri)
	def gba = new Vec3i(gi,bi,ai)
	def gab = new Vec3i(gi,ai,bi)
	def bga = new Vec3i(bi,gi,ai)
	def bag = new Vec3i(bi,ai,gi)
	def agb = new Vec3i(ai,gi,bi)
	def abg = new Vec3i(ai,bi,gi)
	def length : Float = {
		val e = ri*ri + gi*gi + bi*bi + ai*ai
		math.sqrt(e).toFloat
	}
	def lengthSafe : Float = {
		val e = ri*ri + gi*gi + bi*bi + ai*ai
		if ( e != 0 ) { math.sqrt(e).toFloat } else { 0 }
	}
	def abs = new Vec4i(math.abs(r),math.abs(g),math.abs(b),math.abs(a))
	def min(v:ReadVec4i) = new Vec4i(math.min(r, v.r),math.min(g, v.g),math.min(b, v.b),math.min(a, v.a))
	def max(v:ReadVec4i) = new Vec4i(math.max(r, v.r),math.max(g, v.g),math.max(b, v.b),math.max(a, v.a))
	def minMax(v:ReadVec4i) = ( min(v) , max(v) )
	def min = math.min(math.min(math.min(r,g),b),a)
	def max = math.max(math.max(math.max(r,g),b),a)
	def ceil = new Vec4i(math.ceil(r).toInt,math.ceil(g).toInt,math.ceil(b).toInt,math.ceil(a).toInt)
	def floor = new Vec4i(math.floor(r).toInt,math.floor(g).toInt,math.floor(b).toInt,math.floor(a).toInt)
	override def toString = "(" + r + "," + g + "," + b + "," + a+ ")"
	def resolve = this
	def baseValue = this
	override def equals ( other : Any ) = other match {
		case v : ReadVec4i => r == v.r && g == v.g && b == v.b && a == v.a
		case mv : Moddable[ReadVec4i] => this == mv.resolve()
		case _ => false
	}
	override def hashCode = 41 * (41 * (41 * (41 + a.hashCode) + b.hashCode) + g.hashCode) + r.hashCode
	def apply (i:Int) = i match {
		case 0 => r
		case 1 => g
		case 2 => b
		case 3 => a
		case _ => 0
	}

}
object ReadVec4i{
	def apply (ra : Int,ga : Int,ba : Int,aa : Int) = new ReadVec4i(ra : Int,ga : Int,ba : Int,aa : Int)
	def apply (v : ReadVec4i) = new ReadVec4i(v.r,v.g,v.b,v.a)
	def apply (v : ReadVec4f) = new ReadVec4i(v.r.toInt,v.g.toInt,v.b.toInt,v.a.toInt)
	implicit def toFloatingPoint (v : ReadVec4i) = new ReadVec4f(v.r.toFloat,v.g.toFloat,v.b.toFloat,v.a.toFloat)
	def apply (s : Int) = new ReadVec4i(s,s,s,s)
}

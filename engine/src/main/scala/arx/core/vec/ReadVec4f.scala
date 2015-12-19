package arx.core.vec;
import arx.core.Moddable
import arx.core.SelfModdable
import arx.core.mat.ReadMat4x4

@SerialVersionUID(9223372036854770000L)
class ReadVec4f extends InternVec4f with SelfModdable[ReadVec4f]{
	def this(ra : Float,ga : Float,ba : Float,aa : Float){ 
		this()
		ri = ra
		gi = ga
		bi = ba
		ai = aa
	}
	def +(m : Moddable[ReadVec4f]) = { val v = m.resolve(); new ReadVec4f(ri + v.ri,gi + v.gi,bi + v.bi,ai + v.ai) }
	def +(v : ReadVec4f) = new ReadVec4f(ri + v.ri,gi + v.gi,bi + v.bi,ai + v.ai)
	def +(s : Float) = new ReadVec4f(ri + s,gi + s,bi + s,ai + s)
	def -(m : Moddable[ReadVec4f]) = { val v = m.resolve(); new ReadVec4f(ri - v.ri,gi - v.gi,bi - v.bi,ai - v.ai) }
	def -(v : ReadVec4f) = new ReadVec4f(ri - v.ri,gi - v.gi,bi - v.bi,ai - v.ai)
	def -(s : Float) = new ReadVec4f(ri - s,gi - s,bi - s,ai - s)
	def *(m : Moddable[ReadVec4f]) = { val v = m.resolve(); new ReadVec4f(ri * v.ri,gi * v.gi,bi * v.bi,ai * v.ai) }
	def *(v : ReadVec4f) = new ReadVec4f(ri * v.ri,gi * v.gi,bi * v.bi,ai * v.ai)
	def *(s : Float) = new ReadVec4f(ri * s,gi * s,bi * s,ai * s)
	def /(m : Moddable[ReadVec4f]) = { val v = m.resolve(); new ReadVec4f(ri / v.ri,gi / v.gi,bi / v.bi,ai / v.ai) }
	def /(v : ReadVec4f) = new ReadVec4f(ri / v.ri,gi / v.gi,bi / v.bi,ai / v.ai)
	def /(s : Float) = new ReadVec4f(ri / s,gi / s,bi / s,ai / s)
	def r = ri
	protected def r_= ( s : Float ) { ri = s }
	def g = gi
	protected def g_= ( s : Float ) { gi = s }
	def b = bi
	protected def b_= ( s : Float ) { bi = s }
	def a = ai
	protected def a_= ( s : Float ) { ai = s }
	def rg = new Vec2f(ri,gi)
	def gr = new Vec2f(gi,ri)
	def rb = new Vec2f(ri,bi)
	def br = new Vec2f(bi,ri)
	def ra = new Vec2f(ri,ai)
	def ar = new Vec2f(ai,ri)
	def gb = new Vec2f(gi,bi)
	def bg = new Vec2f(bi,gi)
	def ga = new Vec2f(gi,ai)
	def ag = new Vec2f(ai,gi)
	def ba = new Vec2f(bi,ai)
	def ab = new Vec2f(ai,bi)
	def rgb = new Vec3f(ri,gi,bi)
	def rbg = new Vec3f(ri,bi,gi)
	def grb = new Vec3f(gi,ri,bi)
	def gbr = new Vec3f(gi,bi,ri)
	def brg = new Vec3f(bi,ri,gi)
	def bgr = new Vec3f(bi,gi,ri)
	def rga = new Vec3f(ri,gi,ai)
	def rag = new Vec3f(ri,ai,gi)
	def gra = new Vec3f(gi,ri,ai)
	def gar = new Vec3f(gi,ai,ri)
	def arg = new Vec3f(ai,ri,gi)
	def agr = new Vec3f(ai,gi,ri)
	def rba = new Vec3f(ri,bi,ai)
	def rab = new Vec3f(ri,ai,bi)
	def bra = new Vec3f(bi,ri,ai)
	def bar = new Vec3f(bi,ai,ri)
	def arb = new Vec3f(ai,ri,bi)
	def abr = new Vec3f(ai,bi,ri)
	def gba = new Vec3f(gi,bi,ai)
	def gab = new Vec3f(gi,ai,bi)
	def bga = new Vec3f(bi,gi,ai)
	def bag = new Vec3f(bi,ai,gi)
	def agb = new Vec3f(ai,gi,bi)
	def abg = new Vec3f(ai,bi,gi)
	def length : Float = {
		val e = ri*ri + gi*gi + bi*bi + ai*ai
		math.sqrt(e).toFloat
	}
	def lengthSafe : Float = {
		val e = ri*ri + gi*gi + bi*bi + ai*ai
		if ( e != 0 ) { math.sqrt(e).toFloat } else { 0 }
	}
	def abs = new Vec4f(math.abs(r),math.abs(g),math.abs(b),math.abs(a))
	def min(v:ReadVec4f) = new Vec4f(math.min(r, v.r),math.min(g, v.g),math.min(b, v.b),math.min(a, v.a))
	def max(v:ReadVec4f) = new Vec4f(math.max(r, v.r),math.max(g, v.g),math.max(b, v.b),math.max(a, v.a))
	def minMax(v:ReadVec4f) = ( min(v) , max(v) )
	def min = math.min(math.min(math.min(r,g),b),a)
	def max = math.max(math.max(math.max(r,g),b),a)
	def ceil = new Vec4f(math.ceil(r).toFloat,math.ceil(g).toFloat,math.ceil(b).toFloat,math.ceil(a).toFloat)
	def floor = new Vec4f(math.floor(r).toFloat,math.floor(g).toFloat,math.floor(b).toFloat,math.floor(a).toFloat)
	override def toString = "(" + r + "," + g + "," + b + "," + a+ ")"
	def resolve = this
	def baseValue = this
	def normalize = this / length
	def normalizeSafe = {
		val l = lengthSafe
		if ( l != 0 ) { this / l } else { new Vec4f(0,0,0,0) }
	}
	def dot ( v : ReadVec4f ) = (r * v.r+g * v.g+b * v.b+a * v.a)
	def round = new ReadVec4i(r.round,g.round,b.round,a.round)
	override def equals ( other : Any ) = other match {
		case v : ReadVec4f => r == v.r && g == v.g && b == v.b && a == v.a
		case mv : Moddable[ReadVec4f] => this == mv.resolve()
		case _ => false
	}
	override def hashCode = 41 * (41 * (41 * (41 + a.hashCode) + b.hashCode) + g.hashCode) + r.hashCode
	def apply (i:Int) = i match {
		case 0 => r
		case 1 => g
		case 2 => b
		case 3 => a
		case _ => 0.0f
	}


	def *(m : ReadMat4x4) = {
		new ReadVec4f(
			m.m00 * this.r + m.m10 * this.g + m.m20 * this.b + m.m30 * this.a,
			m.m01 * this.r + m.m11 * this.g + m.m21 * this.b + m.m31 * this.a,
			m.m02 * this.r + m.m12 * this.g + m.m22 * this.b + m.m32 * this.a,
			m.m03 * this.r + m.m13 * this.g + m.m23 * this.b + m.m33 * this.a
		)
	}

}
object ReadVec4f{
	def apply (ra : Float,ga : Float,ba : Float,aa : Float) = new ReadVec4f(ra : Float,ga : Float,ba : Float,aa : Float)
	def apply (v : ReadVec4f) = new ReadVec4f(v.r,v.g,v.b,v.a)
	def apply (s : Float) = new ReadVec4f(s,s,s,s)
}

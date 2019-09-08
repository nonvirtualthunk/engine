package arx.core.vec;
import arx.core.Moddable
import arx.core.SelfModdable
import arx.core.traits.TArxNumeric
@SerialVersionUID(9223372036854770000L)
class ReadVec3f extends InternVec3f with SelfModdable[ReadVec3f] with TArxNumeric[ReadVec3f] {
	def this(xa : Float,ya : Float,za : Float){ 
		this()
		xi = xa
		yi = ya
		zi = za
	}
	def zero = Vec3f.Zero
	def +(m : Moddable[ReadVec3f]) = { val v = m.resolve(); new ReadVec3f(xi + v.xi,yi + v.yi,zi + v.zi) }
	def +(v : ReadVec3f) = new ReadVec3f(xi + v.xi,yi + v.yi,zi + v.zi)
	def +(s : Float) = new ReadVec3f(xi + s,yi + s,zi + s)
	def -(m : Moddable[ReadVec3f]) = { val v = m.resolve(); new ReadVec3f(xi - v.xi,yi - v.yi,zi - v.zi) }
	def -(v : ReadVec3f) = new ReadVec3f(xi - v.xi,yi - v.yi,zi - v.zi)
	def -(s : Float) = new ReadVec3f(xi - s,yi - s,zi - s)
	def *(m : Moddable[ReadVec3f]) = { val v = m.resolve(); new ReadVec3f(xi * v.xi,yi * v.yi,zi * v.zi) }
	def *(v : ReadVec3f) = new ReadVec3f(xi * v.xi,yi * v.yi,zi * v.zi)
	def *(s : Float) = new ReadVec3f(xi * s,yi * s,zi * s)
	def /(m : Moddable[ReadVec3f]) = { val v = m.resolve(); new ReadVec3f(xi / v.xi,yi / v.yi,zi / v.zi) }
	def /(v : ReadVec3f) = new ReadVec3f(xi / v.xi,yi / v.yi,zi / v.zi)
	def /(s : Float) = new ReadVec3f(xi / s,yi / s,zi / s)
	def x = xi
	protected def x_= ( s : Float ) { xi = s }
	def y = yi
	protected def y_= ( s : Float ) { yi = s }
	def z = zi
	protected def z_= ( s : Float ) { zi = s }
	def r = xi
	protected def r_= ( s : Float ) { xi = s }
	def g = yi
	protected def g_= ( s : Float ) { yi = s }
	def b = zi
	protected def b_= ( s : Float ) { zi = s }
	def xy = new Vec2f(xi,yi)
	def yx = new Vec2f(yi,xi)
	def xz = new Vec2f(xi,zi)
	def zx = new Vec2f(zi,xi)
	def yz = new Vec2f(yi,zi)
	def zy = new Vec2f(zi,yi)
	def length : Float = {
		val e = xi*xi + yi*yi + zi*zi
		math.sqrt(e).toFloat
	}
	def lengthSafe : Float = {
		val e = xi*xi + yi*yi + zi*zi
		if ( e != 0 ) { math.sqrt(e).toFloat } else { 0 }
	}
	def lengthSquared : Float = {
		xi*xi + yi*yi + zi*zi
	}
	def abs = new Vec3f(math.abs(x),math.abs(y),math.abs(z))
	def min(v:ReadVec3f) = new Vec3f(math.min(x, v.x),math.min(y, v.y),math.min(z, v.z))
	def max(v:ReadVec3f) = new Vec3f(math.max(x, v.x),math.max(y, v.y),math.max(z, v.z))
	def minMax(v:ReadVec3f) = ( min(v) , max(v) )
	def min = math.min(math.min(x,y),z)
	def max = math.max(math.max(x,y),z)
	def ceil = new Vec3f(math.ceil(x).toFloat,math.ceil(y).toFloat,math.ceil(z).toFloat)
	def ceili = new Vec3i(math.ceil(x).toInt,math.ceil(y).toInt,math.ceil(z).toInt)
	def floor = new Vec3f(math.floor(x).toFloat,math.floor(y).toFloat,math.floor(z).toFloat)
	override def toString = "(" + x + "," + y + "," + z+ ")"
	def resolve = this
	def baseValue = this
	def normalize = this / length
	def normalizeSafe = {
		val l = lengthSafe
		if ( l != 0 ) { this / l } else { new Vec3f(0,0,0) }
	}
	def scaleToLength ( l : Float ) = this * ( l / length )
	def dot ( v : ReadVec3f ) = (x * v.x+y * v.y+z * v.z)
	def round = new ReadVec3i(x.round,y.round,z.round)
	def cross ( v : ReadVec3f ) = new Vec3f(y*v.z-v.y*z,z*v.x-v.z*x,x*v.y-v.x*y)
	override def equals ( other : Any ) = other match {
		case v : ReadVec3f => x == v.x && y == v.y && z == v.z
		case mv : Moddable[ReadVec3f] => this == mv.resolve()
		case _ => false
	}
	override def hashCode = 41 * (41 * (41 + z.hashCode) + y.hashCode) + x.hashCode
	def apply (i:Int) = i match {
		case 0 => x
		case 1 => y
		case 2 => z
		case _ => 0.0f
	}

}
object ReadVec3f{
	def apply (xa : Float,ya : Float,za : Float) = new ReadVec3f(xa : Float,ya : Float,za : Float)
	def apply (v : ReadVec3f) = new ReadVec3f(v.x,v.y,v.z)
	def apply (s : Float) = new ReadVec3f(s,s,s)
}

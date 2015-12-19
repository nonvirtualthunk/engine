package arx.core.vec;
import arx.core.Moddable
import arx.core.SelfModdable
@SerialVersionUID(9223372036854770000L)
class ReadVec3i extends InternVec3i with SelfModdable[ReadVec3i]{
	def this(xa : Int,ya : Int,za : Int){ 
		this()
		xi = xa
		yi = ya
		zi = za
	}
	def +(m : Moddable[ReadVec3i]) = { val v = m.resolve(); new ReadVec3i(xi + v.xi,yi + v.yi,zi + v.zi) }
	def +(v : ReadVec3i) = new ReadVec3i(xi + v.xi,yi + v.yi,zi + v.zi)
	def +(s : Int) = new ReadVec3i(xi + s,yi + s,zi + s)
	def -(m : Moddable[ReadVec3i]) = { val v = m.resolve(); new ReadVec3i(xi - v.xi,yi - v.yi,zi - v.zi) }
	def -(v : ReadVec3i) = new ReadVec3i(xi - v.xi,yi - v.yi,zi - v.zi)
	def -(s : Int) = new ReadVec3i(xi - s,yi - s,zi - s)
	def *(m : Moddable[ReadVec3i]) = { val v = m.resolve(); new ReadVec3i(xi * v.xi,yi * v.yi,zi * v.zi) }
	def *(v : ReadVec3i) = new ReadVec3i(xi * v.xi,yi * v.yi,zi * v.zi)
	def *(s : Int) = new ReadVec3i(xi * s,yi * s,zi * s)
	def /(m : Moddable[ReadVec3i]) = { val v = m.resolve(); new ReadVec3i(xi / v.xi,yi / v.yi,zi / v.zi) }
	def /(v : ReadVec3i) = new ReadVec3i(xi / v.xi,yi / v.yi,zi / v.zi)
	def /(s : Int) = new ReadVec3i(xi / s,yi / s,zi / s)
	def %(m : Moddable[ReadVec3i]) = { val v = m.resolve(); new ReadVec3i(xi % v.xi,yi % v.yi,zi % v.zi) }
	def %(v : ReadVec3i) = new ReadVec3i(xi % v.xi,yi % v.yi,zi % v.zi)
	def %(s : Int) = new ReadVec3i(xi % s,yi % s,zi % s)
	def >>(m : Moddable[ReadVec3i]) = { val v = m.resolve(); new ReadVec3i(xi >> v.xi,yi >> v.yi,zi >> v.zi) }
	def >>(v : ReadVec3i) = new ReadVec3i(xi >> v.xi,yi >> v.yi,zi >> v.zi)
	def >>(s : Int) = new ReadVec3i(xi >> s,yi >> s,zi >> s)
	def <<(m : Moddable[ReadVec3i]) = { val v = m.resolve(); new ReadVec3i(xi << v.xi,yi << v.yi,zi << v.zi) }
	def <<(v : ReadVec3i) = new ReadVec3i(xi << v.xi,yi << v.yi,zi << v.zi)
	def <<(s : Int) = new ReadVec3i(xi << s,yi << s,zi << s)
	def x = xi
	protected def x_= ( s : Int ) { xi = s }
	def y = yi
	protected def y_= ( s : Int ) { yi = s }
	def z = zi
	protected def z_= ( s : Int ) { zi = s }
	def r = xi
	protected def r_= ( s : Int ) { xi = s }
	def g = yi
	protected def g_= ( s : Int ) { yi = s }
	def b = zi
	protected def b_= ( s : Int ) { zi = s }
	def xy = new Vec2i(xi,yi)
	def yx = new Vec2i(yi,xi)
	def xz = new Vec2i(xi,zi)
	def zx = new Vec2i(zi,xi)
	def yz = new Vec2i(yi,zi)
	def zy = new Vec2i(zi,yi)
	def length : Float = {
		val e = xi*xi + yi*yi + zi*zi
		math.sqrt(e).toFloat
	}
	def lengthSafe : Float = {
		val e = xi*xi + yi*yi + zi*zi
		if ( e != 0 ) { math.sqrt(e).toFloat } else { 0 }
	}
	def abs = new Vec3i(math.abs(x),math.abs(y),math.abs(z))
	def min(v:ReadVec3i) = new Vec3i(math.min(x, v.x),math.min(y, v.y),math.min(z, v.z))
	def max(v:ReadVec3i) = new Vec3i(math.max(x, v.x),math.max(y, v.y),math.max(z, v.z))
	def minMax(v:ReadVec3i) = ( min(v) , max(v) )
	def min = math.min(math.min(x,y),z)
	def max = math.max(math.max(x,y),z)
	def ceil = new Vec3i(math.ceil(x).toInt,math.ceil(y).toInt,math.ceil(z).toInt)
	def floor = new Vec3i(math.floor(x).toInt,math.floor(y).toInt,math.floor(z).toInt)
	override def toString = "(" + x + "," + y + "," + z+ ")"
	def to ( end : ReadVec3i ) = new ReadVec3i.VecRange(this,end + 1)
	def until ( end : ReadVec3i ) = new ReadVec3i.VecRange(this,end)
	def resolve = this
	def baseValue = this
	override def equals ( other : Any ) = other match {
		case v : ReadVec3i => x == v.x && y == v.y && z == v.z
		case mv : Moddable[ReadVec3i] => this == mv.resolve()
		case _ => false
	}
	override def hashCode = 41 * (41 * (41 + z.hashCode) + y.hashCode) + x.hashCode
	def apply (i:Int) = i match {
		case 0 => x
		case 1 => y
		case 2 => z
		case _ => 0
	}

}
object ReadVec3i{
	def apply (xa : Int,ya : Int,za : Int) = new ReadVec3i(xa : Int,ya : Int,za : Int)
	def apply (v : ReadVec3i) = new ReadVec3i(v.x,v.y,v.z)

	class VecRange(min:ReadVec3i,max:ReadVec3i) extends Traversable[ReadVec3i] {

  		override def size = (max.x - min.x) * (max.y - min.y) * (max.z - min.z)
		def foreach[U](f: (ReadVec3i) => U) {
			if ( min != max ) {
				val current = Vec3i(min)
				var break = false
				while ( ! break ) {
					f(Vec3i(current))
	
					current.x += 1
					if ( current.x >= max.x ) {
						current.x = min.x
						current.y += 1
						if ( current.y >= max.y ) {
							current.y = min.y
							current.z += 1
							if ( current.z >= max.z ) {
								break = true
							}
						}
					}
				}
			}
		}
	}

							def apply (v : ReadVec3f) = new ReadVec3i(v.x.toInt,v.y.toInt,v.z.toInt)
	implicit def toFloatingPoint (v : ReadVec3i) = new ReadVec3f(v.x.toFloat,v.y.toFloat,v.z.toFloat)
	def apply (s : Int) = new ReadVec3i(s,s,s)
}

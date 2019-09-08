package arx.core.vec
import arx.core.Moddable
import arx.core.SelfModdable
import arx.core.traits.TArxNumeric
@SerialVersionUID(9223372036854770000L)
class ReadVec2f extends InternVec2f with SelfModdable[ReadVec2f] with TArxNumeric[ReadVec2f] {
	def this(xa : Float,ya : Float){ 
		this()
		xi = xa
		yi = ya
	}
	def zero = Vec2f.Zero
	def +(m : Moddable[ReadVec2f]) = { val v = m.resolve(); new ReadVec2f(xi + v.xi,yi + v.yi) }
	def +(v : ReadVec2f) = new ReadVec2f(xi + v.xi,yi + v.yi)
	def +(s : Float) = new ReadVec2f(xi + s,yi + s)
	def -(m : Moddable[ReadVec2f]) = { val v = m.resolve(); new ReadVec2f(xi - v.xi,yi - v.yi) }
	def -(v : ReadVec2f) = new ReadVec2f(xi - v.xi,yi - v.yi)
	def -(s : Float) = new ReadVec2f(xi - s,yi - s)
	def *(m : Moddable[ReadVec2f]) = { val v = m.resolve(); new ReadVec2f(xi * v.xi,yi * v.yi) }
	def *(v : ReadVec2f) = new ReadVec2f(xi * v.xi,yi * v.yi)
	def *(s : Float) = new ReadVec2f(xi * s,yi * s)
	def /(m : Moddable[ReadVec2f]) = { val v = m.resolve(); new ReadVec2f(xi / v.xi,yi / v.yi) }
	def /(v : ReadVec2f) = new ReadVec2f(xi / v.xi,yi / v.yi)
	def /(s : Float) = new ReadVec2f(xi / s,yi / s)
	def x = xi
	protected def x_= ( s : Float ) { xi = s }
	def y = yi
	protected def y_= ( s : Float ) { yi = s }
	def xy = this
	def length : Float = {
		val e = xi*xi + yi*yi
		math.sqrt(e).toFloat
	}
	def lengthSafe : Float = {
		val e = xi*xi + yi*yi
		if ( e != 0 ) { math.sqrt(e).toFloat } else { 0 }
	}
	def abs = new Vec2f(math.abs(x),math.abs(y))
	def min(v:ReadVec2f) = new Vec2f(math.min(x, v.x),math.min(y, v.y))
	def max(v:ReadVec2f) = new Vec2f(math.max(x, v.x),math.max(y, v.y))
	def minMax(v:ReadVec2f) = ( min(v) , max(v) )
	def min = math.min(x,y)
	def max = math.max(x,y)
	def ceil = new Vec2f(math.ceil(x).toFloat,math.ceil(y).toFloat)
	def floor = new Vec2f(math.floor(x).toFloat,math.floor(y).toFloat)
	override def toString = "(" + x + "," + y+ ")"
	def resolve = this
	def baseValue = this
	def normalize = this / length
	def normalizeSafe = {
		val l = lengthSafe
		if ( l != 0 ) { this / l } else { new Vec2f(0,0) }
	}
	def dot ( v : ReadVec2f ) = (x * v.x+y * v.y)
	def cross ( v : ReadVec2f ) = y * v.x - x * v.y
	def round = new ReadVec2i(x.round,y.round)
	override def equals ( other : Any ) = other match {
		case v : ReadVec2f => x == v.x && y == v.y
		case mv : Moddable[ReadVec2f] => this == mv.resolve()
		case _ => false
	}
	override def hashCode = 41 * (41 + y.hashCode) + x.hashCode
	def apply (i:Int) = i match {
		case 0 => x
		case 1 => y
		case _ => 0.0f
	}

}
object ReadVec2f{
	def apply (xa : Float,ya : Float) = new ReadVec2f(xa : Float,ya : Float)
	def apply (v : ReadVec2f) = new ReadVec2f(v.x,v.y)
	def apply (s : Float) = new ReadVec2f(s,s)
}

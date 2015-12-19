package arx.core.vec;
import arx.core.Moddable
import arx.core.SelfModdable
@SerialVersionUID(9223372036854770000L)
class ReadVec2i extends InternVec2i with SelfModdable[ReadVec2i]{
	def this(xa : Int,ya : Int){ 
		this()
		xi = xa
		yi = ya
	}
	def +(m : Moddable[ReadVec2i]) = { val v = m.resolve(); new ReadVec2i(xi + v.xi,yi + v.yi) }
	def +(v : ReadVec2i) = new ReadVec2i(xi + v.xi,yi + v.yi)
	def +(s : Int) = new ReadVec2i(xi + s,yi + s)
	def -(m : Moddable[ReadVec2i]) = { val v = m.resolve(); new ReadVec2i(xi - v.xi,yi - v.yi) }
	def -(v : ReadVec2i) = new ReadVec2i(xi - v.xi,yi - v.yi)
	def -(s : Int) = new ReadVec2i(xi - s,yi - s)
	def *(m : Moddable[ReadVec2i]) = { val v = m.resolve(); new ReadVec2i(xi * v.xi,yi * v.yi) }
	def *(v : ReadVec2i) = new ReadVec2i(xi * v.xi,yi * v.yi)
	def *(s : Int) = new ReadVec2i(xi * s,yi * s)
	def /(m : Moddable[ReadVec2i]) = { val v = m.resolve(); new ReadVec2i(xi / v.xi,yi / v.yi) }
	def /(v : ReadVec2i) = new ReadVec2i(xi / v.xi,yi / v.yi)
	def /(s : Int) = new ReadVec2i(xi / s,yi / s)
	def %(m : Moddable[ReadVec2i]) = { val v = m.resolve(); new ReadVec2i(xi % v.xi,yi % v.yi) }
	def %(v : ReadVec2i) = new ReadVec2i(xi % v.xi,yi % v.yi)
	def %(s : Int) = new ReadVec2i(xi % s,yi % s)
	def >>(m : Moddable[ReadVec2i]) = { val v = m.resolve(); new ReadVec2i(xi >> v.xi,yi >> v.yi) }
	def >>(v : ReadVec2i) = new ReadVec2i(xi >> v.xi,yi >> v.yi)
	def >>(s : Int) = new ReadVec2i(xi >> s,yi >> s)
	def <<(m : Moddable[ReadVec2i]) = { val v = m.resolve(); new ReadVec2i(xi << v.xi,yi << v.yi) }
	def <<(v : ReadVec2i) = new ReadVec2i(xi << v.xi,yi << v.yi)
	def <<(s : Int) = new ReadVec2i(xi << s,yi << s)
	def x = xi
	protected def x_= ( s : Int ) { xi = s }
	def y = yi
	protected def y_= ( s : Int ) { yi = s }
	def length : Float = {
		val e = xi*xi + yi*yi
		math.sqrt(e).toFloat
	}
	def lengthSafe : Float = {
		val e = xi*xi + yi*yi
		if ( e != 0 ) { math.sqrt(e).toFloat } else { 0 }
	}
	def abs = new Vec2i(math.abs(x),math.abs(y))
	def min(v:ReadVec2i) = new Vec2i(math.min(x, v.x),math.min(y, v.y))
	def max(v:ReadVec2i) = new Vec2i(math.max(x, v.x),math.max(y, v.y))
	def minMax(v:ReadVec2i) = ( min(v) , max(v) )
	def min = math.min(x,y)
	def max = math.max(x,y)
	def ceil = new Vec2i(math.ceil(x).toInt,math.ceil(y).toInt)
	def floor = new Vec2i(math.floor(x).toInt,math.floor(y).toInt)
	override def toString = "(" + x + "," + y+ ")"
	def to ( end : ReadVec2i ) = new ReadVec2i.VecRange(this,end + 1)
	def until ( end : ReadVec2i ) = new ReadVec2i.VecRange(this,end)
	def resolve = this
	def baseValue = this
	override def equals ( other : Any ) = other match {
		case v : ReadVec2i => x == v.x && y == v.y
		case mv : Moddable[ReadVec2i] => this == mv.resolve()
		case _ => false
	}
	override def hashCode = 41 * (41 + y.hashCode) + x.hashCode
	def apply (i:Int) = i match {
		case 0 => x
		case 1 => y
		case _ => 0
	}

}
object ReadVec2i{
	def apply (xa : Int,ya : Int) = new ReadVec2i(xa : Int,ya : Int)
	def apply (v : ReadVec2i) = new ReadVec2i(v.x,v.y)

	class VecRange(min:ReadVec2i,max:ReadVec2i) extends Traversable[ReadVec2i] {

  		override def size = (max.x - min.x) * (max.y - min.y)
		def foreach[U](f: (ReadVec2i) => U) {
			if ( min != max ) {
   			val current = Vec2i(min)
				var break = false
				while ( ! break ) {
					f(Vec2i(current))
	
					current.x += 1
					if ( current.x >= max.x ) {
						current.x = min.x
						current.y += 1
						if ( current.y >= max.y ) {
							break = true
						}
					}
				}
			}
		}
	}

							def apply (v : ReadVec2f) = new ReadVec2i(v.x.toInt,v.y.toInt)
	implicit def toFloatingPoint (v : ReadVec2i) = new ReadVec2f(v.x.toFloat,v.y.toFloat)
	def apply (s : Int) = new ReadVec2i(s,s)
}

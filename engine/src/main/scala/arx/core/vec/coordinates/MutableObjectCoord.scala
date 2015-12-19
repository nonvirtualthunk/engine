package arx.core.vec.coordinates

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 11/11/12
 * Time: 9:48 PM
 * Created by nonvirtualthunk
 */

import arx.core.Moddable
import arx.core.vec.{ReadVec3f, Vec3f}

class MutableObjectCoord(xa:Float,ya:Float,za:Float) extends Vec3f(xa,ya,za) with ObjectCoord {
	def this () { this ( 0.0f,0.0f,0.0f ) }


	override def + ( mv : Moddable[ReadVec3f] ) : MutableObjectCoord = { val v = mv.resolve(); new MutableObjectCoord(x + v.x,y + v.y,z + v.z) }
	override def - ( mv : Moddable[ReadVec3f] ) : MutableObjectCoord = { val v = mv.resolve(); new MutableObjectCoord(x - v.x,y - v.y,z - v.z) }

	override def + ( v : ReadVec3f ) : MutableObjectCoord = { new MutableObjectCoord(x + v.x,y + v.y,z + v.z) }
	override def - ( v : ReadVec3f ) : MutableObjectCoord = { new MutableObjectCoord(x - v.x,y - v.y,z - v.z) }
	override def / ( f : Float ) : MutableObjectCoord = { new MutableObjectCoord(x/f,y/f,z/f) }
	override def * ( f : Float ) : MutableObjectCoord = { new MutableObjectCoord(x*f,y*f,z*f) }

	override def plusZ ( f : Float ) : MutableObjectCoord = { new MutableObjectCoord(x,y,z + f) }
	override def minusZ ( f : Float ) : MutableObjectCoord = { new MutableObjectCoord(x,y,z - f) }
	override def plusX ( f : Float ) : MutableObjectCoord = { new MutableObjectCoord(x+f,y,z) }
	override def minusX ( f : Float ) : MutableObjectCoord = { new MutableObjectCoord(x-f,y,z) }
	override def plusY ( f : Float ) : MutableObjectCoord = { new MutableObjectCoord(x,y+f,z) }
	override def minusY ( f : Float ) : MutableObjectCoord = { new MutableObjectCoord(x,y-f,z) }
}

object MutableObjectCoord {
	def apply ( o : ObjectCoord ) = new MutableObjectCoord(o.x,o.y,o.z)
	def apply ( x : Float, y : Float ,z : Float ) = new MutableObjectCoord(x,y,z)
}
package arx.core.vec.coordinates

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 11/11/12
 * Time: 10:33 PM
 * Created by nonvirtualthunk
 */

import arx.core.vec.ReadVec3f

class ConstObjectCoord(xa:Float,ya:Float,za:Float) extends ReadVec3f(xa,ya,za) with ObjectCoord{
	def this () { this(0.0f,0.0f,0.0f) }
}
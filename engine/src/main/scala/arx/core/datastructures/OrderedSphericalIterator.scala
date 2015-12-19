package arx.core.datastructures

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/13/15
 * Time: 6:43 PM
 */

import arx.Prelude._
import arx.core.vec.{Vec3i, ReadVec3i}
import scalaxy.loops._

/**
 * Iterates over all relative coordinates from 0,0,0 outward in a three dimensional
 * spiral. Will always examine closer points before farther points, stops when it
 * hits a point that is farther from the center than radius, inclusive. (3,0,0) will
 * be hit if given radius 3, for example.
 * <br />
 * <strong>Important Note!</strong> the <code>Vec3i</code> should be treated as
 * unsafe and read-only! Never store a reference to that vec without making a
 * defensive copy.
 */
class OrderedSphericalIterator(radius : Int) extends Iterator[Vec3i] {
	val r2 = radius * radius
	private final val Q = new RingBuffer[Int]
	val ret = Vec3i(0,0,0)
	
	protected def addSuccessors ( dx : Int , dy : Int , dz : Int ) {
		if ( dx == 0 && dy == 0 && dz == 0 ) { addV(dx+1,dy,dz) ; addV(dx,dy+1,dz) ; addV(dx-1,dy,dz) ; addV(dx,dy-1,dz) ; addV(dx,dy,dz+1) ; addV(dx,dy,dz-1) }
		else if ( dx == 0 && dy == 0 && dz < 0 ) { addV(dx+1,dy,dz) ; addV(dx,dy+1,dz) ; addV(dx-1,dy,dz) ; addV(dx,dy-1,dz) ; addV(dx,dy,dz-1) }
		else if ( dx == 0 && dy == 0 && dz > 0 ) { addV(dx+1,dy,dz) ; addV(dx,dy+1,dz) ; addV(dx-1,dy,dz) ; addV(dx,dy-1,dz) ; addV(dx,dy,dz+1) }
		else if ( dx > 0 && dy >= 0 ) { addV(dx,dy+1,dz) }
		else if ( dx <= 0 && dy > 0 ) { addV(dx-1,dy,dz) }
		else if ( dx < 0 && dy <= 0 ) { addV(dx,dy-1,dz) }
		else if ( dx >= 0 && dy < 0 ) { addV(dx+1,dy,dz) }

		if ( dx == 0 && dy > 0 ) { addV(dx,dy+1,dz) }
		else if ( dx == 0 && dy < 0 ) { addV(dx,dy-1,dz) }
		else if ( dx > 0 && dy == 0 ) { addV(dx+1,dy,dz) }
		else if ( dx < 0 && dy == 0 ) { addV(dx-1,dy,dz) }
	}

	protected def addV ( x : Int , y : Int , z : Int ) {
		if (x*x+y*y+z*z <= r2) {
			Q.enqueue(x)
			Q.enqueue(y)
			Q.enqueue(z)
		}
	}

	override def hasNext: Boolean = Q.nonEmpty
	override def next(): Vec3i = {
		val x = Q.dequeue()
		val y = Q.dequeue()
		val z = Q.dequeue()

		ret.x = x
		ret.y = y
		ret.z = z

		addSuccessors(x,y,z)

		ret
	}

	if (radius >= 0) {
		addV(0,0,0)
	}
}

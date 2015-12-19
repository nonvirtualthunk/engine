package arx.core.stream

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/14/13
 * Time: 1:48 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import java.io.{ObjectStreamClass, OutputStream, ObjectOutputStream}

class TrackingOutputStream(wrapping:OutputStream) extends ObjectOutputStream(wrapping) {
	var track = true
	var allTypes = Set[Class[_]]()


//	override def writeUnshared(p1 : Any): Unit = {
//		if (track) {
//			allTypes += p1.getClass
//		}
//
//		super.writeUnshared ()
//	}

	override def writeClassDescriptor(p1: ObjectStreamClass): Unit = {
		if (track) {
			allTypes += p1.forClass()
		}

		super.writeClassDescriptor (p1)
	}


}
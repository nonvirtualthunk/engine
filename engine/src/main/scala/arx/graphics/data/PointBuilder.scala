package arx.graphics.data

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/29/16
  * Time: 7:13 PM
  */

import java.nio.ByteBuffer

import arx.Prelude._
import arx.graphics.AttributeProfile
import arx.graphics.GL
import scalaxy.loops._
import arx.core.vec._

class PointBuilder(attribProf : AttributeProfile) {
	var bytes : ByteBuffer = GL.createByteBuffer(attribProf.byteStride)
}

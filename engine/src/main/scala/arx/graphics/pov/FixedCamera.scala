package arx.graphics.pov

import arx.core.vec.ReadVec3f

class FixedCamera(eye_ : ReadVec3f, forward_ : ReadVec3f, up_ : ReadVec3f) extends EyeCamera(eye_, forward_, up_) {
	override def manualUpdate(): Unit = {}
}

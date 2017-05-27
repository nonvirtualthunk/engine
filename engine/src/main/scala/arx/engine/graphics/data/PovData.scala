package arx.engine.graphics.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.graphics.pov.EyeCamera
import arx.graphics.pov.TCamera

import scalaxy.loops._

class PovData extends TGraphicsData {
	var pov : TCamera = new EyeCamera()
}

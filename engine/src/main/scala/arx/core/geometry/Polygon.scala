package arx.core.geometry

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 12/20/15
  * Time: 3:10 PM
  */

import arx.Prelude._
import scalaxy.loops._
import arx.core.vec._

class Polygon extends TShape {
	var points = List[ReadVec3f]()
}

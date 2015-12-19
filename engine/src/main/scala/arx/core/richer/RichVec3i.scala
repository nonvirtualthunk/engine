package arx.core.richer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 10:24 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.vec.ReadVec3i
import arx.core.vec.Cardinals._

class RichVec3i( v : ReadVec3i) {
	def adjacents = for ( i <- 0 until 6 ) yield {
		ReadVec3i(v.x + cardinalsX(i),v.y + cardinalsY(i),v.z + cardinalsZ(i))
	}
}
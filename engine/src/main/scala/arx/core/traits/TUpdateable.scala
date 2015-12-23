package arx.core.traits

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 5:14 PM
 */

import arx.Prelude._
import arx.core.units.UnitOfTime
import scalaxy.loops._

trait TUpdateable {
	def updateSelf(dt : UnitOfTime): Unit = {
		update(dt)
	}

	protected def update (dt : UnitOfTime)
}

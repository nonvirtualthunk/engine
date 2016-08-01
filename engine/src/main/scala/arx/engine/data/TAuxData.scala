package arx.engine.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 8:19 AM
 */

import java.util.concurrent.atomic.AtomicInteger

import arx.Prelude._
import scalaxy.loops._

trait TAuxData {
	def onAssignedToObject ( entity : THasAuxData[_] ) {}
}

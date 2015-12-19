package arx.core.traits

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/8/13
 * Time: 10:28 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto

trait TSentinelable {
	def isSentinel = false
	def notSentinel = ! isSentinel
}

trait TSentinel extends TSentinelable {
	override def isSentinel = true

}
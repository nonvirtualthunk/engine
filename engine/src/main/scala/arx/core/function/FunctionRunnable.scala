package arx.core.function

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/27/13
 * Time: 9:53 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto

class FunctionRunnable(func : () => Unit) extends Runnable {
	def run() {
		func()
	}
}
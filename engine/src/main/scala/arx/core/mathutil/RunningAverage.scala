package arx.core.mathutil

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 10:21 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto

class RunningAverage {
	def rebase() {
		val a = avg
		count = count / 2
		sum = a * count.toFloat
	}

	var sum = 0.0f
	var count = 0

	def add ( value : Float ) { sum += value ; count += 1 }
	def avg = count match { case 0 => 0.0f ; case nonZero => sum / nonZero.toFloat }
}
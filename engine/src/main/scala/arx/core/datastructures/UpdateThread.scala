package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/30/12
 * Time: 10:34 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import java.util.concurrent.locks.LockSupport
import arx.core.units.UnitOfTime

abstract class UpdateThread(interval : UnitOfTime,operationLevel:Int = Killable.ApplicationLevel) extends KillableThread(operationLevel) {
	var rawInterval = interval.inSeconds
	var timeElapsed = 0.0f
	var parkTime = 5000000

//	var parkNanos = (interval * 0.25f).inNanoseconds

	def timePassed ( time : UnitOfTime ) {
		timeElapsed += time.inSeconds
		LockSupport.unpark(this)
	}

	override def whileRunningDo() {
		if ( timeElapsed > rawInterval ) {
			update()
			timeElapsed -= rawInterval
		} else {
			//LockSupport.parkNanos(((rawInterval - timeElapsed) / 1000000000.0).toLong)
			LockSupport.parkNanos(5000000) // 5 milis
		}
	}

	def update ()

	start()
}
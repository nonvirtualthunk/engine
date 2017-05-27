package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/30/12
 * Time: 12:54 PM
 * Created by nonvirtualthunk
 */

import java.util.concurrent.locks.LockSupport

import arx.Prelude._
import arx.application.Noto

abstract class KillableThread(var level:Int) extends Thread {
	KillableThread.threads ::= this

	var ended = false
	var finishComplete = false

	final override def run() {
		while ( ! ended ) {
			whileRunningDo()
		}
		finishComplete = true
	}

	def whileRunningDo ()

	def end () {
		kill()
	}
	def kill () {
		if ( ! ended ) {
			ended = true
			interrupt()

			while (!finishComplete) {
				LockSupport.parkNanos(1000000)
			}
		}
	}

	def andStart() : this.type = {
		start()
		this
	}
}
object KillableThread {
	var threads = List[KillableThread]()

	def kill (level:Int=10) {
		threads.foreach ( t => if ( t.level <= level ) { t.kill() } )
	}
}
object Killable {
	def kill (level : Int = 10) {
		KillableThread.kill(level)
	}

	val ApplicationLevel = 10
	val GameLevel = 5
	val TemporaryLevel = 1
}

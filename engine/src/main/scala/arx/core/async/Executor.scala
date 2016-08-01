package arx.core.async

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/18/15
 * Time: 9:23 AM
 */

import java.util.concurrent.TimeUnit
import java.util.concurrent.{Callable, Executors}

import arx.Prelude._
import arx.application.Application
import arx.core.datastructures.SynchronizedQueue
import arx.core.units.UnitOfTime

import scalaxy.loops._

object Executor {
	val threadpool = Executors.newCachedThreadPool()
	val scheduledThreadpool = Executors.newSingleThreadScheduledExecutor()

	def submitAsync[T](func : () => T) = {
		threadpool.submit(new Callable[T] {
			override def call(): T = func()
		})
	}

	def submitAsync(runnable : Runnable): Unit = {
		threadpool.submit(runnable)
	}

	def submitAsync[T](callable : Callable[T]) = {
		threadpool.submit(callable)
	}

	def schedulePeriodic(interval : UnitOfTime, func : () => Unit) = {
		scheduledThreadpool.schedule(func,interval.inMilliseconds.toLong, TimeUnit.MILLISECONDS)
	}

	def shutDownThreadPool () {
		threadpool.shutdownNow()
	}

	def onQuit (): Unit = {
		shutDownThreadPool()
	}
}
//
//class SubmitQueue {
//	val queue = new SynchronizedQueue[() => Any]
//
//	def submit[T] (func : () => T) : Unit = {
//		val fut = Async.threadpool.submit(new Callable[T] {
//			override def call(): T = {
//				val ret = func()
//
//			}
//		})
//		fut.
//	}
//}

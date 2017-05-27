package arx.core.traits

/**
  * TODO: Add javadoc
  */

import java.util.concurrent.atomic.AtomicBoolean

import arx.Prelude._

import scalaxy.loops._

trait TBoolLockable {
	val boolLock = new AtomicBoolean(false)
}

package arx.core

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/5/12
 * Time: 4:59 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.{Application, Noto}
import java.io.{ObjectInputStream, IOException}

@SerialVersionUID(1L)
class CachedValue[T <: AnyRef] ( f : => T , ticksBetweenUpdates : Int = 100 ) extends Serializable {
	@transient var lastValue : T = null.asInstanceOf[T]
	@transient var lastTick = -10000
	def resolve () : T = {
		if ( lastValue == null || Application.ticks - lastTick > ticksBetweenUpdates ) {
			lastValue = f
			lastTick = Application.ticks
		}
		lastValue
	}
	def update() { lastTick = -1000; resolve() }
}
class CachedBoolean ( f : => Boolean , ticksBetweenUpdates : Int = 100 ) extends Serializable {
	@transient var lastValue : Boolean = false
	@transient var lastTick = -10000
	var initialized = false
	def resolve () : Boolean = {
		if ( ! initialized || Application.ticks - lastTick > ticksBetweenUpdates ) {
			initialized = true
			lastValue = f
			lastTick = Application.ticks
		}
		lastValue
	}
	def update() { lastTick = -1000; resolve() }
}
class CachedFloat ( f : => Float , ticksBetweenUpdates : Int = 100 ) extends Serializable {
	@transient var lastValue : Float = 0.0f
	@transient var lastTick = -10000
	var initialized = false
	def resolve () : Float = {
		if ( ! initialized || Application.ticks - lastTick > ticksBetweenUpdates ) {
			initialized = true
			lastValue = f
			lastTick = Application.ticks
		}
		lastValue
	}
	def update() { lastTick = -1000; resolve() }
}
class CachedInt ( f : => Int , ticksBetweenUpdates : Int = 100 ) extends Moddable[Int] with Serializable {
	@transient var lastValue : Int = 0
	@transient var lastTick = -10000
	var initialized = false
	def resolve () : Int = {
		if ( ! initialized || Application.ticks - lastTick > ticksBetweenUpdates ) {
			initialized = true
			lastValue = f
			lastTick = Application.ticks
		}
		lastValue
	}

	def baseValue() = resolve()
	override def dynamic = false
def update() { lastTick = -1000; resolve() }
}

class CachedModdable[T <: AnyRef] ( f : => T , ticksBetweenUpdates : Int = 100 ) extends Moddable[T] {
	@transient var lastValue : T = null.asInstanceOf[T]
	@transient var lastTick = -10000

	def baseValue() = resolve()
	def resolve () : T = {
		if ( lastValue == null || Application.ticks - lastTick > ticksBetweenUpdates ) {
			lastValue = f
			lastTick = Application.ticks
		}
		lastValue
	}
	def update() { lastTick = -1000; resolve() }
}
class CachedModdableFloat ( f : => Float , ticksBetweenUpdates : Int = 100 ) extends Moddable[Float] {
	@transient var lastValue : Float = 0.0f
	@transient var lastTick = -10000
	var initialized = false
	def baseValue() = resolve()
	def resolve () : Float = {
		if ( ! initialized || Application.ticks - lastTick > ticksBetweenUpdates ) {
			initialized = true
			lastValue = f
			lastTick = Application.ticks
		}
		lastValue
	}
	def update() { lastTick = -1000; resolve() }
}
class CachedModdableInt ( f : => Int , ticksBetweenUpdates : Int = 100 ) extends Moddable[Int] {
	@transient var lastValue : Int = 0
	@transient var lastTick = -10000
	var initialized = false
	def baseValue() = resolve()
	def resolve () : Int = {
		if ( ! initialized || Application.ticks - lastTick > ticksBetweenUpdates ) {
			initialized = true
			lastValue = f
			lastTick = Application.ticks
		}
		lastValue
	}
	def update() { lastTick = -1000; resolve() }
}

object CachedValue {
	def apply ( b : => Boolean , ticksBetweenUpdates : Int ) = new CachedBoolean(b,ticksBetweenUpdates)
	def apply[T <: AnyRef] ( f : => T , ticksBetweenUpdates : Int = 100 ) = new CachedValue(f,ticksBetweenUpdates)
	implicit def cachedToValue[T <: AnyRef] ( c : CachedValue[T] ) = c.resolve()

	var _offset = 0
	def offset = { _offset += 1 ; _offset }
}
object CachedFloat {
	implicit def cachedToValue ( c : CachedFloat ) = c.resolve()
}
object CachedInt {
	implicit def cachedToValue ( c : CachedInt ) = c.resolve()
}
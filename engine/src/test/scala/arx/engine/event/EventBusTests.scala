package arx.engine.event

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import org.scalatest.FlatSpec

import scala.util.Random
import scalaxy.loops._

class EventBusTests extends FlatSpec {

	case class FooEvent(i : Int) extends GameEvent
	case class BarEvent(s : String) extends GameEvent


	"Event Bus in normal situation" should "allow an event to be processed by multiple readers" in {
		val bus = new EventBus

		val listener1 = bus.createListener()
		val listener2 = bus.createListener()

		var seen1 = false
		var seen2 = false
		var seenSyncBar = false;
		var seenSyncFoo = false;

		bus.onEvent {
			case BarEvent(b) => seenSyncBar = true
			case FooEvent(f) => seenSyncFoo = true
		}
		listener1.onEvent {
			case FooEvent(i) => seen1 = true
		}
		listener2.onEvent {
			case FooEvent(i) => seen2 = true
		}

		bus.fireEvent(FooEvent(5))
		bus.fireEvent(BarEvent("hello"))

		listener1.process()
		listener2.process()

		require(seen1)
		require(seen2)
		require(seenSyncBar)
		require(seenSyncFoo)
	}

	"An empty Event Bus" should "cause nothing to happen with its listeners" in {
		val bus = new EventBus

		val listener = bus.createListener()
		listener.onEvent {
			case BarEvent(b) => throw new IllegalStateException("BAD")
		}

		listener.process()
	}

	"An Event Bus with many events" should "be properly consumed without crashing" in {
		val bus = new EventBus

		var allValues = Set[Int]()
		var seen = Set[Int]()

		val listener = bus.createListener()
		listener.onEvent {
			case FooEvent(f) => seen += f
		}

		val random = new Random()
		for (i <- 0 until bus.sizeLimit) {
			for (j <- 0 until random.nextInt(5)+1) {
				val value = random.nextInt()
				bus.fireEvent(FooEvent(value))
				allValues += value
			}
			listener.process()
		}

		require(allValues.diff(seen).isEmpty)
	}
}

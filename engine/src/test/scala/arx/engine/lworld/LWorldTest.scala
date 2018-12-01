package arx.engine.lworld

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/15/18
  * Time: 1:20 PM
  */

import arx.Prelude._
import arx.core.introspection.Field
import scalaxy.loops._
import arx.core.vec._
import arx.engine.data.TAuxData
import arx.engine.event.GameEvent
import org.scalatest.FlatSpec

class FooData extends TAuxData {
	var a : Int = 0
	var b : Float = 0.0f
	var nested : Nested = new Nested
	var nestedMap : Map[AnyRef, Nested] = Map()
}

case class Nested() {
	var x : Int = 1
	var y : Int = 1
}

object FooData {
	val Sentinel = new FooData
	val a = Field.fromValue(Sentinel.a).createField[FooData]("i",f => f.a, (f,i) => f.a = i)
	val b = Field.fromValue(Sentinel.b).createField[FooData]("s",f => f.b, (f,s) => f.b = s)
	val nested = Field.fromValue(Sentinel.nested).createField[FooData]("nested", f => f.nested, (f,nested) => f.nested = nested)
	val nestedMap = Field.fromValue(Sentinel.nestedMap).createField[FooData]("nestedMap", f => f.nestedMap, (f,nestedMap) => f.nestedMap = nestedMap)
}

object Nested {
	val Sentinel = new Nested
	val x = Field.fromValue(Sentinel.x).createField[Nested]("x", f => f.x, (f,x) => f.x = x)
	val y = Field.fromValue(Sentinel.y).createField[Nested]("y", f => f.y, (f,y) => f.y = y)
}

case class TestEvent(i : Int) extends GameEvent(None) {

}

class LWorldTest extends FlatSpec {
	import arx.core.introspection.FieldOperations._

	"Ledger world" should "be able to add data and retrieve it from views" in {
		val world = new LWorld

		world.register[FooData]()

		val entity = world.createEntity()

		world.attachData(entity, new FooData)

		implicit val view = world.view

		val foo = entity[FooData]

		assert(foo.a == 0)
		assert(foo.b == 0.0f)

		world.modify(entity, FooData.a + 3)
		world.endEvent(TestEvent(0))
		assert(world.currentTime == GameEventClock(0))

		assert(foo.a == 3)

		val entity2 = world.createEntity()

		world.attachData(entity2, new FooData)

		val foo2 = entity2[FooData]

		assert(foo2.a == 0)

		world.modify(entity2, FooData.b + 1.0f)
		world.endEvent(TestEvent(1))

		assert(foo2.b == 1.0f)

		val viewAt1 = world.viewAtTime(world.currentTime - 1)

		val foo2At1 = viewAt1.data[FooData](entity2)

		assert(viewAt1.events.size == 1)
		assert(foo2At1.b == 0.0f)
	}
	
	"Ledger world" should "be able to support nested field modifications" in {
		val world = new LWorld

		world.register[FooData]()

		val entity = world.createEntity()
		val baseData = new FooData
		baseData.nestedMap = Map("test1" -> new Nested, "test2" -> new Nested)
		world.attachData(entity, baseData)

		implicit val view : LWorldView = world.view

		val foo = entity[FooData]

		assert(foo.nested == Nested.Sentinel)
		val oldNestedRef = foo.nested

		world.modify(entity, NestedModifier(FooData.nested, Nested.x + 3))
		world.addEvent(TestEvent(1))

		assert(foo.nested.x == 4)
		assert(oldNestedRef.x == 1)

		assert(foo.nestedMap("test1").x == 1)

		world.modify(entity, NestedKeyedModifier(FooData.nestedMap, "test1", Nested.y + 1))
		world.addEvent(TestEvent(2))

		assert(foo.nestedMap("test1").y == 2)
		assert(foo.nestedMap("test2").y == 1)
	}
}

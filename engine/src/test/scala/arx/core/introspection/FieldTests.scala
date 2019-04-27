package arx.core.introspection

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/27/18
  * Time: 8:17 AM
  */

import arx.Prelude._
import arx.core.macros.GenerateCompanion
import scalaxy.loops._
import arx.core.vec._
import arx.engine.data.TAuxData
import org.scalatest.FlatSpec


@GenerateCompanion
class Thimble extends TAuxData {
	var i : Int = 0
	var s : Seq[String] = Seq()
}

object Thimble extends Clazz[Thimble]("Thimble", classOf[Thimble]) {
	val Sentinel = new Thimble
	val i = Field.fromValue(Sentinel.i).createField[Thimble]("i",f => f.i, (f,i) => f.i = i, Thimble)
	fields += "i" -> i
	val s = Field.fromValue(Sentinel.s).createField[Thimble]("s",f => f.s, (f,s) => f.s = s, Thimble)
	fields += "s" -> s
}

class FieldTests extends FlatSpec {

	import FieldOperations._

	"A field" should "be able to create operations off of itself" in {
		val operation = Thimble.i + 2

		require(operation.operation.asSimpleString == "+ 2")

		val setOperation = Thimble.i -> 3

		require(setOperation.operation.asSimpleString == "= 3")


		val inst = new Thimble
		operation(inst)
		assert(inst.i == 2)
		setOperation(inst)
		assert(inst.i == 3)
	}

	"A seq field" should "be able to perform collection operations" in {
		val appendHello = Thimble.s.append("hello")
		val appendWorld = Thimble.s.append("world")

		val inst = new Thimble
		appendHello(inst)
		assert(inst.s == Seq("hello"))
		appendWorld(inst)
		assert(inst.s == Seq("hello", "world"))
	}
}

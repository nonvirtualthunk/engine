package arx.core.introspection

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/19/15
 * Time: 2:20 PM
 */

import arx.Prelude._
import com.esotericsoftware.kryo.io.ByteBufferInput
import com.esotericsoftware.kryo.io.ByteBufferOutput
import com.esotericsoftware.kryo.io.Output
import com.twitter.chill.ScalaKryoInstantiator
import scalaxy.loops._

object CopyAssistant {
	val kryo = new ScalaKryoInstantiator().newKryo()

	def copy[T <: AnyRef] (inst : T) : T = {
		val out = new ByteBufferOutput(100,1000000)
		kryo.writeClassAndObject(out, inst)
		val in = new ByteBufferInput(out.toBytes)
		kryo.readClassAndObject(in).asInstanceOf[T]
//		kryo.copy(inst)
	}
	def copyShallow[T <: AnyRef] (inst : T) : T = {
		kryo.copyShallow(inst)
	}
}

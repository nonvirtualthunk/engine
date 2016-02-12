package arx.core.serialization

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/23/15
 * Time: 7:48 AM
 */

import arx.core.datastructures.AtomicMapi
import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output

import scalaxy.loops._

object AtomicMapiSerializer extends ArxKryoSerializer[AtomicMapi[_,_]]{
	override def write(kryo: Kryo, output: Output, t: AtomicMapi[_, _]): Unit = {
		output.writeInt(t.size)
		for ((k,v) <- t) {
			kryo.writeClassAndObject(output,k)
			kryo.writeClassAndObject(output,v)
		}
	}
	override def read(kryo: Kryo, input: Input, aClass: Class[AtomicMapi[_, _]]): AtomicMapi[_, _] = {
		val size = input.readInt
		val ret = new AtomicMapi[AnyRef,AnyRef]()
		for (i <- 0 until size optimized) {
			val k = kryo.readClassAndObject(input)
			val v = kryo.readClassAndObject(input)
			ret.put(k,v)
		}
		ret
	}
}

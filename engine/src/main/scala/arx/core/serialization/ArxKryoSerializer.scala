package arx.core.serialization

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/19/15
 * Time: 4:27 PM
 */

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer

abstract class ArxKryoSerializer[T : Manifest] extends Serializer[T] with TArxKryoSerializerBase[T] {
	def forClasses : Seq[Class[_]] = List(manifest[T].runtimeClass)
	def forNetworked : Boolean = false
}

abstract class ArxKryoNetworkSerializer[T : Manifest] extends Serializer[T] with TArxKryoSerializerBase[T] {
	def forClasses : Seq[Class[_]] = List(manifest[T].runtimeClass)
	def forNetworked : Boolean = true
}

trait TArxKryoSerializerBase[T] extends Serializer[T] {
	def forClasses : Seq[Class[_]]
	def forNetworked : Boolean
}

trait TArxKryoSubRegistrar {
	def register(kryo : Kryo)
}
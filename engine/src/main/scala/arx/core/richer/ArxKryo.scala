package arx.core.richer

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/25/15
 * Time: 11:38 AM
 */

import arx.Prelude._
import arx.core.introspection.ReflectionAssistant
import arx.core.introspection.IntrospectionUtil
import arx.core.serialization.ArxKryoRegistrar

//import arx.serialization.{ArxKryoRegistrar, ArxConnection}
import com.esotericsoftware.kryo.serializers.DeflateSerializer
import com.esotericsoftware.kryo.{Serializer, Kryo}
import com.esotericsoftware.kryo.util.ObjectMap
import com.twitter.chill.AllScalaRegistrar
import scalaxy.loops._

class ArxKryo(val kryo : Kryo) extends AnyVal{
	def userData (k : Any) : Any = {
		kryo.getContext.asInstanceOf[ObjectMap[Any,Any]].get(k)
	}

	def userData[T] (k : Any, defV : T) : T = {
		kryo.getContext.asInstanceOf[ObjectMap[Any,T]].get(k,defV)
	}

	def setUserData[K,V] (k : K, v : V): Unit = {
		kryo.getContext.asInstanceOf[ObjectMap[K,V]].put(k,v)
	}

	def registerSpecialized (clazz : Class[_], serializer : Serializer[_]): Unit = {
		for (subClass <- IntrospectionUtil.specializedFormsOf(clazz)) {
			kryo.register(subClass,serializer)
		}
	}

	def registerForCompression (clazz : Class[_]): Unit = {
		val reg = kryo.getRegistration(clazz)
//		if (kryo.register())
		kryo.register(clazz,new DeflateSerializer(reg.getSerializer))
	}

	def registerForCompressionSpecialized (clazz : Class[_]) = {
		for (subClass <- IntrospectionUtil.specializedFormsOf(clazz)) {
			registerForCompression(subClass)
		}
	}

	def registerClassAndAllSubclasses (clazz : Class[_], serializer : Serializer[_]): Unit = {
		val allSubclasses = ReflectionAssistant.allSubTypesOf(clazz)
		for (subClass <- clazz :: allSubclasses) {
			kryo.register(subClass,serializer)
		}
	}

	// TODO: re-enable...if we get back to using entities and whatnot
//	def connection = this.userData("connection").asInstanceOf[ArxConnection]
//
//
	def applyArxDefaultSettings (): Unit = {
		kryo.setRegistrationRequired(false)
		kryo.setInstantiatorStrategy(new org.objenesis.strategy.StdInstantiatorStrategy)
		new AllScalaRegistrar().apply(kryo)
		ArxKryoRegistrar.register(kryo,networked = true)
	}
}

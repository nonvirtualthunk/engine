package arx.core.serialization

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/19/15
 * Time: 4:29 PM
 */

import arx.core.introspection.ReflectionAssistant
import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.minlog.Log

import scala.collection.mutable

object ArxKryoRegistrar {
	def register (kryo : Kryo, networked : Boolean): Unit = {
		val registeredClasses = new mutable.HashSet[Class[_]]()

		val serializers = ReflectionAssistant.instancesOfSubtypesOf[TArxKryoSerializerBase[_]].filter(! _.forNetworked || networked)

		for (inst <- serializers) {
			for (forClass <- inst.forClasses if ! registeredClasses.contains(forClass)) {
				kryo.register(forClass,inst)
				registeredClasses.add(forClass)
				Log.trace("serialization",s"Registering ${inst.getClass.getSimpleName} as serializer for ${forClass.getSimpleName}")
			}
		}

		val subRegistrars = ReflectionAssistant.instancesOfSubtypesOf[TArxKryoSubRegistrar]
		for (subReg <- subRegistrars) {
			subReg.register(kryo)
		}
	}
}

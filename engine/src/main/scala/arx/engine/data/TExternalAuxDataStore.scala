package arx.engine.data

import arx.core.introspection.ReflectionAssistant
import com.carrotsearch.hppc.LongObjectOpenHashMap

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 8:58 AM
 */

trait TExternalAuxDataStore[U <: TAuxData] {
	def getAuxDataOrElse[T <: U](id : Long, clazz : Class[T], orElse : T) : T

	def storeAuxData[T <: U](id : Long, obj : T)

	def removeAuxData[T <: U](id : Long, clazz : Class[T])

	def allAuxData(id : Long) : List[U]
}

class MapBackedExternalAuxDataStore[U <: TAuxData : Manifest] extends TExternalAuxDataStore[U] {
	val backingClasses = ReflectionAssistant.allSubTypesOf[U].toSet
	val backingMaps = Array.fill(backingClasses.size)(new LongObjectOpenHashMap[U])
	val indices = backingClasses.zipWithIndex.toMap

	override def getAuxDataOrElse[T <: U](id: Long, clazz: Class[T], orElse: T): T = {
		backingMaps(indices(clazz)).get(id) match {
			case null => orElse
			case nonNull => nonNull.asInstanceOf[T]
		}
	}

	override def storeAuxData[T <: U](id: Long, obj: T): Unit = {
		backingMaps(indices(obj.getClass)).put(id, obj)
	}

	override def removeAuxData[T <: U](id: Long, clazz: Class[T]): Unit = {
		backingMaps(indices(clazz)).remove(id)
	}

	override def allAuxData(id: Long): List[U] = {
		var ret : List[U] = Nil
		for (map <- backingMaps) {
			map.get(id) match {
				case null => // do nothing
				case nonNull => ret ::= nonNull
			}
		}
		ret
	}
}
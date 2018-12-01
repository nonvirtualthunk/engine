package arx.engine.lworld

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/13/18
  * Time: 7:30 AM
  */

import java.util.concurrent.atomic.AtomicLong

import arx.Prelude._
import arx.application.Noto
import arx.core.introspection.{CopyAssistant, Field, ReflectionAssistant, Transformation}
import arx.core.traits.TSentinelable
import scalaxy.loops._
import arx.core.vec._
import arx.engine.data.TAuxData
import arx.engine.event.GameEvent
import com.carrotsearch.hppc.LongObjectOpenHashMap
import overlock.atomicmap.AtomicMap

import scala.reflect.ClassTag

class LEntity(val id : Long) extends AnyVal {
	def data[T <: TAuxData](implicit view : LWorldView, tag : ClassTag[T]) : T = view.data[T](this)
	def apply[T <: TAuxData](implicit view : LWorldView, tag : ClassTag[T]) : T = view.data[T](this)
	def hasData[T <: TAuxData](implicit view : LWorldView, tag : ClassTag[T]) : Boolean = view.hasData[T](this)
	def has[T <: TAuxData](implicit view : LWorldView, tag : ClassTag[T]) : Boolean = view.hasData[T](this)
}
case class GameEventClock(time : Int) extends AnyVal {
	def < (other : GameEventClock) : Boolean = this.time < other.time
	def > (other : GameEventClock) : Boolean = this.time > other.time
	def <= (other : GameEventClock) : Boolean = this.time <= other.time
	def >= (other : GameEventClock) : Boolean = this.time >= other.time
	def - (value : Int) : GameEventClock = GameEventClock(this.time - value)
	def + (value : Int) : GameEventClock = GameEventClock(this.time + value)
}


trait Modifier[T] {
	val tag : ClassTag[T]

	def apply(value : T) : Unit

	def applyUntyped(value : AnyRef): Unit = {
		this.apply(value.asInstanceOf[T])
	}
}
case class LambdaModifier[T](function : T => Unit)(implicit val tag : ClassTag[T]) extends Modifier[T] {
	def apply(value : T) : Unit = {
		function(value)
	}
}
case class FieldOperationModifier[C, T](field : Field[C,T], operation : Transformation[T])(implicit val tag : ClassTag[C]) extends Modifier[C] {
	def apply(dataObj : C): Unit = {
		val oldValue = field.getter(dataObj)
		val newValue = operation.apply(oldValue)
		field.setter(dataObj, newValue)
	}
}
case class NestedModifier[C,T <: AnyRef,V](topField : Field[C,T], nestedOperation : Modifier[T])(implicit val tag : ClassTag[C]) extends Modifier[C]{
	override def apply(dataObject: C): Unit = {
		val nestedData = topField.getter(dataObject)
		val clonedData = CopyAssistant.copy(nestedData)
		nestedOperation.apply(clonedData)
		topField.setter(dataObject, clonedData)
	}
}
case class NestedKeyedModifier[C, K, V <: AnyRef, NV](topField : Field[C, Map[K, V]], key : K, nestedModifier : Modifier[V])(implicit val tag : ClassTag[C]) extends Modifier[C] {
	override def apply(dataObject: C): Unit = {
		val nestedMap = topField.getter(dataObject)
		nestedMap.get(key) match {
			case Some(nestedData) =>
				val clonedData = CopyAssistant.copy(nestedData)
				nestedModifier.apply(clonedData)
				val newMap = nestedMap + (key -> clonedData)
				topField.setter(dataObject, newMap)
			case None => Noto.warn(s"Modifier attempted to modify nested key $key, but that was not present in map, doing nothing")
		}
	}
}


class Modification(val modifiedType : Class[_], val entity : LEntity, val modifier : Modifier[_])
class ModifierReference(protected[engine] val index : Int) extends AnyVal {}

sealed abstract class EventState {}
object EventState {
	case object Started extends EventState {}
	case object Continues extends EventState {}
	case object Ended extends EventState {}
}

protected[engine] class EventWrapper(val event : GameEvent, val occurredAt : GameEventClock, val state : EventState, val modificationIndexLimit : Int)
protected[engine] class EntityWrapper(val entity : LEntity, val addedAt : GameEventClock)
protected[engine] class EntityDataWrapper[T](val data : T, val addedAt : GameEventClock)
protected[engine] case class EntityDataRegistration(entity : LEntity, dataType : Class[_], atTime : GameEventClock)

class LWorld {
	protected val coreView : LWorldView = new LWorldView()
	protected val currentView : LWorldView = new LWorldView()
	protected val entityCounter = new AtomicLong(0)
	protected var dataRegistrations = Vector[EntityDataRegistration]()
	protected val selfEntity : LEntity = createEntity()

	var onEntityAddedCallbacks = List[LEntity => Unit]()
	var onEntityRemovedCallbacks = List[LEntity => Unit]()

	def nextTime = coreView.nextTime
	def currentTime = coreView.currentTime

	def register[T <: TAuxData]()(implicit tag : ClassTag[T]) : Unit = {
		coreView.dataStores += tag.runtimeClass -> new EntityDataStore[T]()
		currentView.dataStores += tag.runtimeClass -> new EntityDataStore[T]()
	}

	def createEntity() : LEntity = {
		val newId = entityCounter.incrementAndGet()
		val newEnt = new LEntity(newId)
		coreView.entities :+= new EntityWrapper(newEnt, nextTime)
		currentView.entities = coreView.entities

		onEntityAddedCallbacks.foreach(c => c(newEnt))

		newEnt
	}

	def attachData[T <: TAuxData] (entity : LEntity, data : T)(implicit tag : ClassTag[T]) : Unit = {
		for (view <- List(coreView, currentView)) {
			view.dataStores.get(tag.runtimeClass) match {
				case Some(dataStore) => {
					dataStore.putUntyped(entity, data, currentTime)
				}
				case None => throw new IllegalStateException(s"Types must be registered with a world before attaching to entity, ${tag.runtimeClass} was not")
			}
		}

		dataRegistrations :+= EntityDataRegistration(entity, tag.runtimeClass, currentTime)
	}

	def modify[T](entity : LEntity, modifier : Modifier[T])(implicit tag : ClassTag[T]) : ModifierReference = {
		val index = coreView.modifications.size
		coreView.modifications :+= new Modification(tag.runtimeClass, entity, modifier)
		currentView.modifications = coreView.modifications

		currentView.applyModification(coreView.modifications.last)

		new ModifierReference(index)
	}

	def view : LWorldView = this.currentView

	def viewAtTime(time : GameEventClock): LWorldView = {
		val newView = coreView.copyAtTime(time)
		newView.nextDataRegistrationsIndex = dataRegistrations.lastIndexWhere(r => r.atTime <= time) + 1
		newView
	}

	def updateViewToTime(view : LWorldView, time : GameEventClock) : Unit = {
		val newDataRegistartions = dataRegistrations.slice(view.nextDataRegistrationsIndex, dataRegistrations.size)
			.takeWhile(r => r.atTime <= time)

		newDataRegistartions
   		.foreach(registration => {
				val data = CopyAssistant.copyShallow(coreView.dataStoreForClass(registration.dataType).getUntyped(registration.entity))
				view.dataStoreForClass(registration.dataType).putUntyped(registration.entity, data, time)
			})

		view.nextDataRegistrationsIndex += newDataRegistartions.size

		val nextModificationStart = view.events.lastOption.map(e => e.modificationIndexLimit).getOrElse(0)
		val newEvents = coreView.events.slice(view.nextTime.time, time.time)
		view.events ++= newEvents
		val nextModificationLimit = view.events.lastOption.map(e => e.modificationIndexLimit).getOrElse(0)

		val newModifications = coreView.modifications.slice(nextModificationStart, nextModificationLimit)
		newModifications.foreach(mod => view.applyModification(mod))
		view.modifications ++= newModifications
	}

	protected[engine] def pushEvent(event : GameEvent, state : EventState): Unit = {
		val time = nextTime
		val eventWrapper = new EventWrapper(event, time, state, coreView.modifications.size)
		coreView.events :+= eventWrapper
		currentView.events = coreView.events
	}

	def startEvent(event : GameEvent): Unit = { this.pushEvent(event, EventState.Started) }
	def continueEvent(event : GameEvent) : Unit = { this.pushEvent(event, EventState.Continues) }
	def endEvent(event : GameEvent) : Unit = { this.pushEvent(event, EventState.Ended) }
	def addEvent(event : GameEvent) : Unit = { this.pushEvent(event, EventState.Started); this.pushEvent(event, EventState.Ended) }
}

class LWorldView {
	protected[engine] var dataStores = Map[Class[_], EntityDataStore[_ <: TAuxData]]()
	protected[engine] var events = Vector[EventWrapper]()
	protected[engine] var entities = Vector[EntityWrapper]()
	protected[engine] var modifications = Vector[Modification]()

	protected[engine] var nextDataRegistrationsIndex = 0

	def nextTime = GameEventClock(events.size)
	def currentTime = GameEventClock(nextTime.time - 1)

	protected[engine] def dataStoreForClass(clazz : Class[_]) : EntityDataStore[_] = {
		dataStores.get(clazz) match {
			case Some(store) => store
			case None => throw new IllegalStateException(s"Types must be registered with a world before use, ${clazz} was not")
		}
	}

	def dataStore[T <: TAuxData](implicit tag : ClassTag[T]) : EntityDataStore[T] = {
		dataStores.get(tag.runtimeClass) match {
			case Some(store) => store.asInstanceOf[EntityDataStore[T]]
			case None => throw new IllegalStateException(s"Types must be registered with a world before use, ${tag.runtimeClass} was not")
		}
	}

	def data[T <: TAuxData](entity : LEntity)(implicit tag : ClassTag[T]) : T = {
		dataStores.get(tag.runtimeClass) match {
			case Some(store) => store.get(entity).asInstanceOf[T]
			case None => throw new IllegalStateException(s"Types must be registered with a world before use, ${tag.runtimeClass} was not")
		}
	}

	def dataOpt[T <: TAuxData](entity : LEntity)(implicit tag : ClassTag[T]) : Option[T] = {
		dataStores.get(tag.runtimeClass).flatMap(store => store.getOpt(entity).asInstanceOf[Option[T]])
	}

	def hasData[T <: TAuxData](entity : LEntity)(implicit tag : ClassTag[T]) : Boolean = {
		dataStores.get(tag.runtimeClass) match {
			case Some(store) => store.contains(entity)
			case None => false
		}
	}

	protected[engine] def applyModification(modification : Modification): Unit = {
		val data = dataStores(modification.modifiedType).get(modification.entity)
		modification.modifier.applyUntyped(data)
	}

	protected[engine] def copyAtTime(atTime : GameEventClock) : LWorldView = {
		val view = new LWorldView()
		view.events = this.events.filter(e => e.occurredAt <= atTime)
		view.entities = this.entities.filter(e => e.addedAt <= atTime)

		val entitySet = view.entities.map(e => e.entity.id).toSet
		view.dataStores = this.dataStores.map { case (k,v) => (k, v.copyForEntitiesAtTime(entitySet, atTime).asInstanceOf[EntityDataStore[_ <: TAuxData]]) }.toMap

		val upToModificationIndex = view.events.lastOption.map(e => e.modificationIndexLimit).getOrElse(0)
		view.modifications = this.modifications.take(upToModificationIndex)
		view.modifications.foreach(m => view.applyModification(m))

		view
	}
}


class EntityDataStore[T](implicit tag : ClassTag[T]) {
	val values = AtomicMap.atomicNBHM[Long, EntityDataWrapper[T]]
	val sentinel = ReflectionAssistant.instantiate(tag.runtimeClass.asInstanceOf[Class[T]])

	protected[engine] def putUntyped(entity : LEntity, data : AnyRef, time : GameEventClock) : Unit = {
		put(entity, data.asInstanceOf[T], time)
	}

	def put(entity : LEntity, data : T, time : GameEventClock) : Unit = {
		values.put(entity.id, new EntityDataWrapper[T](data, time))
	}

	def getOpt(entity : LEntity) : Option[T] = {
		values.get(entity.id).map(v => v.data)
	}

	def get(entity : LEntity) : T = {
		values.get(entity.id).map(v => v.data).getOrElse(sentinel)
	}

	def contains(entity : LEntity) : Boolean = {
		values.contains(entity.id)
	}

	protected[engine] def getUntyped(entity : LEntity) : AnyRef = {
		this.get(entity).asInstanceOf[AnyRef]
	}

	def copyForEntitiesAtTime(entityIdsSet : Set[Long], atTime : GameEventClock) : this.type = {
		val newDataStore = new EntityDataStore[T]()

		values.foreach { case (entityId, value) => {
			if (entityIdsSet(entityId) && value.addedAt <= atTime) {
				newDataStore.values.put(entityId, CopyAssistant.copyShallow(value))
			}
		}}

		newDataStore.asInstanceOf[this.type]
	}
}

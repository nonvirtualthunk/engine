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
import arx.core.introspection._
import arx.core.traits.{ArxGeneric, TSentinelable}
import scalaxy.loops._
import arx.core.vec._
import arx.engine.data.{TAuxData, TDynamicAuxData}
import arx.engine.event.GameEvent
import com.carrotsearch.hppc.LongObjectOpenHashMap
import overlock.atomicmap.AtomicMap

import scala.reflect.ClassTag

case class LEntity(val id : Long) {
	def data[T <: TAuxData](implicit view : LWorldView, tag : ClassTag[T]) : T = view.data[T](this)
	def data[T <: TAuxData](clazz : Clazz[T])(implicit view : LWorldView) : T = view.data[T](clazz)(this)
	def dataOpt[T <: TAuxData](implicit view : LWorldView, tag : ClassTag[T]) : Option[T] = view.dataOpt[T](this)
	def apply[T <: TAuxData](implicit view : LWorldView, tag : ClassTag[T]) : T = view.data[T](this)
	def apply[T <: TAuxData](clazz : Clazz[T])(implicit view : LWorldView) : T = view.data[T](clazz)(this)
	def hasData[T <: TAuxData](implicit view : LWorldView, tag : ClassTag[T]) : Boolean = view.hasData[T](this)
	def has[T <: TAuxData](implicit view : LWorldView, tag : ClassTag[T]) : Boolean = view.hasData[T](this)

	override def toString: String = s"LEntity($id)"
}
object LEntity {
	val Sentinel = new LEntity(-1L)
}
case class GameEventClock(time : Int) extends AnyVal {
	def < (other : GameEventClock) : Boolean = this.time < other.time
	def > (other : GameEventClock) : Boolean = this.time > other.time
	def <= (other : GameEventClock) : Boolean = this.time <= other.time
	def >= (other : GameEventClock) : Boolean = this.time >= other.time
	def - (value : Int) : GameEventClock = GameEventClock(this.time - value)
	def + (value : Int) : GameEventClock = GameEventClock(this.time + value)

	def asInt = time
}


trait Modifier[T] {
	val tag : ClassTag[T]

	def apply(value : T) : Unit

	def applyUntyped(value : AnyRef): Unit = {
		this.apply(value.asInstanceOf[T])
	}

	def description : String

	def impact : Impact
}
case class LambdaModifier[T](function : T => Unit, description : String, impact : Impact)(implicit val tag : ClassTag[T]) extends Modifier[T] {
	def apply(value : T) : Unit = {
		function(value)
	}
}
case class  FieldOperationModifier[C, T](field : Field[C,T], operation : Transformation[T])(implicit val tag : ClassTag[C]) extends Modifier[C] {
	def apply(dataObj : C): Unit = {
		val oldValue = field.getter(dataObj)
		val newValue = operation.apply(oldValue)
		field.setter(dataObj, newValue)
	}

	def description = operation.asSimpleString
	def impact = operation.impact
}
case class NestedModifier[C,T <: AnyRef,V](topField : Field[C,T], nestedOperation : Modifier[T])(implicit val tag : ClassTag[C]) extends Modifier[C]{
	override def apply(dataObject: C): Unit = {
		val nestedData = topField.getter(dataObject)
		val clonedData = CopyAssistant.copy(nestedData)
		nestedOperation.apply(clonedData)
		topField.setter(dataObject, clonedData)
	}

	def description = nestedOperation.description
	def impact = nestedOperation.impact
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

	def description = nestedModifier.description
	def impact = nestedModifier.impact
}


class Modification(val modifiedType : Class[_], val entity : LEntity, val modifier : Modifier[_], val source : Option[String], var toggles : List[(GameEventClock, Boolean)] = Nil) {
	def isActiveAt(time : GameEventClock) : Boolean = if (toggles.isEmpty) {
		true
	} else {
		toggles.takeWhile(t => t._1 <= time).lastOption.map(t => t._2).getOrElse(true)
	}
}
object Modification {
	def apply[C](entity : LEntity, modifier : Modifier[C], source : Option[String])(implicit tag : ClassTag[C]) : Modification = {
		new Modification(tag.runtimeClass, entity, modifier, source)
	}
}
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
	protected val entityCounter = new AtomicLong(0)
	protected var dataRegistrations = Vector[EntityDataRegistration]()

	var onEntityAddedCallbacks = List[LEntity => Unit]()
	var onEntityRemovedCallbacks = List[LEntity => Unit]()

	protected val coreView : LWorldView = new LWorldView(this)
	protected val currentView : LWorldView = new LWorldView(this)

	protected val selfEntity : LEntity = createEntity()

	coreView.selfEntity = selfEntity
	currentView.selfEntity = selfEntity

	DebugWorld.world = this

	def nextTime = coreView.nextTime
	def currentTime = coreView.currentTime

	// todo: rfind
	def eventAt(eventClock : GameEventClock) = coreView.events.find(e => e.occurredAt == eventClock)

	def register[T <: TAuxData]()(implicit tag : ClassTag[T]) : Unit = {
		coreView.dataStores += tag.runtimeClass -> new EntityDataStore[T](tag.runtimeClass.asInstanceOf[Class[T]])
		currentView.dataStores += tag.runtimeClass -> new EntityDataStore[T](tag.runtimeClass.asInstanceOf[Class[T]])
	}

	def registerClass(clazz : Class[_ <: TAuxData]) : Unit = {
		coreView.dataStores += clazz -> new EntityDataStore(clazz)
		currentView.dataStores += clazz -> new EntityDataStore(clazz)
	}

	def registerSubtypesOf[T <: TAuxData]()(implicit tag : ClassTag[T]) : Unit = {
		ReflectionAssistant.allSubTypesOf(tag.runtimeClass).foreach(clazz => registerClass(clazz.asInstanceOf[Class[_ <: TAuxData]]))
	}

	def createEntity(providedId : Long = -1) : LEntity = {
		val newId = if (providedId == -1) { entityCounter.incrementAndGet() } else { providedId }
		val newEnt = new LEntity(newId)
		coreView.entities :+= new EntityWrapper(newEnt, nextTime)
		currentView.entities = coreView.entities

		onEntityAddedCallbacks.foreach(c => c(newEnt))

		newEnt
	}

	def attachData (entity : LEntity) = {
		new AttachDataBuilder(this, entity)
	}

	def attachData[T <: TAuxData] (entity : LEntity, data : T)(implicit tag : ClassTag[T]) : Unit = {
		for (view <- List(coreView, currentView)) {
			view.dataStores.get(tag.runtimeClass) match {
				case Some(dataStore) => {
					val dataToProvide = if (!(view eq coreView)) {
						CopyAssistant.copy(data)
					} else {
						data
					}
					dataStore.putUntyped(entity, dataToProvide, currentTime)
				}
				case None => {
					register[T]()
					attachData[T](entity, data)
				}
			}
		}

		dataRegistrations :+= EntityDataRegistration(entity, tag.runtimeClass, currentTime)
	}

	def attachDataWith[T <: TAuxData] (entity : LEntity, dataInit : T => Unit)(implicit tag : ClassTag[T]) : Unit = {
		val newData = ReflectionAssistant.instantiate(tag.runtimeClass.asInstanceOf[Class[T]])
		dataInit(newData)
		attachData(entity, newData)
	}

	def attachWorldData[T <: TAuxData] (data : T)(implicit tag : ClassTag[T]) : Unit = {
		attachData[T](selfEntity, data)
	}

	def modify[T](entity : LEntity, modifier : Modifier[T], source : Option[String])(implicit tag : ClassTag[T]) : ModifierReference = {
		val index = coreView.modifications.size
		coreView.modifications :+= new Modification(tag.runtimeClass, entity, modifier, source)
		currentView.modifications = coreView.modifications

		currentView.applyModification(coreView.modifications.last)

		new ModifierReference(index)
	}

	def modify[T](entity : LEntity, modifier : Modifier[T], source : String)(implicit tag : ClassTag[T]) : ModifierReference = {
		modify(entity, modifier, Some(source))
	}

	def modifyWorld[T](modifier : Modifier[T], source : Option[String])(implicit tag : ClassTag[T]) : ModifierReference = {
		modify(selfEntity, modifier, source)
	}

	def view : LWorldView = this.currentView

	def viewAtTime(time : GameEventClock): LWorldView = {
		val newView = coreView.copyAtTime(time)
		newView.nextDataRegistrationsIndex = dataRegistrations.lastIndexWhere(r => r.atTime <= time) + 1
		newView
	}

	/**
	  * Update the given view to the given time. That is, after updating the final event applied in
	  * the view will have the given time.
	  */
	def updateViewToTime(view : LWorldView, time : GameEventClock) : Unit = {
		val newDataRegistrations = dataRegistrations.slice(view.nextDataRegistrationsIndex, dataRegistrations.size)
			.takeWhile(r => r.atTime <= time)

		newDataRegistrations
   		.foreach(registration => {
				val data = CopyAssistant.copy(coreView.dataStoreForClass(registration.dataType).getUntyped(registration.entity))
				view.dataStoreForClass(registration.dataType).putUntyped(registration.entity, data, registration.atTime)
			})

		view.nextDataRegistrationsIndex += newDataRegistrations.size

		val nextModificationStart = view.events.lastOption.map(e => e.modificationIndexLimit).getOrElse(0)
		val newEvents = coreView.events.slice(view.nextTime.time, time.asInt+1) // +1 because we want to go up to and include the given time
		view._events ++= newEvents
		val nextModificationLimit = view.events.lastOption.map(e => e.modificationIndexLimit).getOrElse(0)

		val newModifications = coreView.modifications.slice(nextModificationStart, nextModificationLimit)
		newModifications.foreach(mod => view.applyModification(mod))
		view.modifications ++= newModifications
	}

	protected[engine] def pushEvent(event : GameEvent, state : EventState): Unit = {
		val time = nextTime
		val eventWrapper = new EventWrapper(event, time, state, coreView.modifications.size)
		coreView._events :+= eventWrapper
		currentView._events = coreView.events
	}

	def startEvent(event : GameEvent): Unit = { this.pushEvent(event, EventState.Started) }
	def continueEvent(event : GameEvent) : Unit = { this.pushEvent(event, EventState.Continues) }
	def endEvent(event : GameEvent) : Unit = { this.pushEvent(event, EventState.Ended) }
	def addEvent(event : GameEvent) : Unit = {
		this.pushEvent(event, EventState.Started)
		this.pushEvent(event, EventState.Ended)
	}

	def dataModificationLog[C <: TAuxData](entity : LEntity)(implicit tag : ClassTag[C]) = {
		val rawData = coreView.data[C](entity)
		currentView.dataModificationLog[C](entity, rawData)
	}
}

class AttachDataBuilder(world : LWorld, entity : LEntity) {
	def ofType[T <: TAuxData](init : T => Unit)(implicit tag : ClassTag[T]) = {
		world.attachDataWith[T](entity,init)
		this
	}
}

class LWorldView(val world : LWorld) {
	protected[engine] var dataStores = Map[Class[_], EntityDataStore[_ <: TAuxData]]()
	protected[engine] var _events = Vector[EventWrapper]()
	protected[engine] var entities = Vector[EntityWrapper]()
	protected[engine] var modifications = Vector[Modification]()

	protected[engine] var nextDataRegistrationsIndex = 0
	protected[engine] var selfEntity : LEntity = LEntity.Sentinel

	def nextTime = GameEventClock(events.size)
	def currentTime = GameEventClock(nextTime.time - 1)

	def events = this._events

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

	def data[T <: TAuxData](clazz : Clazz[T])(entity : LEntity) : T = {
		dataStores.get(clazz.runtimeClass) match {
			case Some(store) => store.get(entity).asInstanceOf[T]
			case None => throw new IllegalStateException(s"Types must be registered with a world before use, ${clazz.runtimeClass} was not")
		}
	}

	def worldData[T <: TAuxData](implicit tag : ClassTag[T]) : T = {
		data[T](selfEntity)(tag)
	}

	def dataOpt[T <: TAuxData](entity : LEntity)(implicit tag : ClassTag[T]) : Option[T] = {
		dataStores.get(tag.runtimeClass).flatMap(store => store.getOpt(entity).asInstanceOf[Option[T]])
	}

	def dataOptByClass[T](entity : LEntity, clazz : Class[T]) : Option[T] = {
		dataStores.get(clazz).flatMap(store => store.getOpt(entity)).asInstanceOf[Option[T]]
	}

	def hasData[T <: TAuxData](entity : LEntity)(implicit tag : ClassTag[T]) : Boolean = {
		hasDataByClass[T](entity, tag.runtimeClass)
	}
	@inline
	def hasDataByClass[T <: TAuxData](entity : LEntity, runtimeClass : Class[_]) : Boolean = {
		dataStores.get(runtimeClass) match {
			case Some(store) => store.contains(entity)
			case None => false
		}
	}

	def applyOverlayModification(modification : Modification) : Unit = {
		val dataStore = dataStores(modification.modifiedType)
		// get the existing overlay data if possible, and if not get a copy of the original
		val existingDataOpt = dataStore.getOverlayOpt(modification.entity)
		val data = existingDataOpt.getOrElse(CopyAssistant.copy(dataStore.get(modification.entity)))
		modification.modifier.applyUntyped(data)

		dataStore.putOverlayUntyped(modification.entity, data)
	}

	def clearOverlay(): Unit = {
		dataStores.values.foreach(ds => ds.clearOverlay())
	}

	protected[engine] def applyModification(modification : Modification): Unit = {
		val data = dataStores(modification.modifiedType).get(modification.entity)
		modification.modifier.applyUntyped(data)
	}

	protected[engine] def copyAtTime(atTime : GameEventClock) : LWorldView = {
		val view = new LWorldView(world)
		view.selfEntity = selfEntity
		view._events = this.events.filter(e => e.occurredAt <= atTime)
		view.entities = this.entities.filter(e => e.addedAt <= atTime)

		val entitySet = view.entities.map(e => e.entity.id).toSet
		view.dataStores = this.dataStores.map { case (k,v) => (k, v.copyForEntitiesAtTime(entitySet, atTime).asInstanceOf[EntityDataStore[_ <: TAuxData]]) }.toMap

		val upToModificationIndex = view.events.lastOption.map(e => e.modificationIndexLimit).getOrElse(0)
		view.modifications = this.modifications.take(upToModificationIndex)
		view.modifications.foreach(m => view.applyModification(m))

		view
	}

	def dataModificationLog[C <: TAuxData](entity : LEntity, baseValue : C)(implicit tag : ClassTag[C]) = {
		val curValue = CopyAssistant.copy(baseValue)
		var breakdownByField = Map[Field[_,_], Vector[BreakdownElement[Any]]]()

		for (m <- modifications.filter(m => m.entity == entity && m.modifiedType == tag.runtimeClass && m.isActiveAt(currentTime))) {
			m.modifier match {
				case FieldOperationModifier(field, operation) =>
					val oldFieldValue = field.getValue(curValue)
					m.modifier.applyUntyped(curValue)
					val newFieldValue = field.getValue(curValue)

					val impact = Impact.fromBeforeAfterAny(oldFieldValue, newFieldValue).getOrElse(m.modifier.impact)

					val existingElements = breakdownByField.getOrElse(field, Vector())
					breakdownByField += field -> (existingElements :+ BreakdownElement(m.source, m.modifier.description, impact))
				case NestedKeyedModifier(topField, key, nestedModifier) =>
					m.modifier.applyUntyped(curValue)

					val existingElements = breakdownByField.getOrElse(topField, Vector())
					breakdownByField += topField -> (existingElements :+ BreakdownElement(m.source, s"[$key] + ${m.modifier.description}", nestedModifier.impact))
				case _ =>
					Noto.error("Non-field operations not fully supported in dataModificationLog")
					m.modifier.applyUntyped(curValue)
			}
		}

		val breakdowns : Map[Field[_,_], Breakdown[Any]] = breakdownByField.map { case (field, elements) => field -> Breakdown[Any](field.getValue(curValue), elements) }.toMap
		new DataModificationLog[C](baseValue, curValue, breakdowns)
	}

	def dataModificationLog[C <: TAuxData](entity : LEntity)(implicit tag : ClassTag[C]) : DataModificationLog[C] = {
		world.dataModificationLog[C](entity)
	}

	def worldCached[X](compute : => X) = new WorldCachedValue(this, compute)
}

class WorldCachedValue[T](worldView : LWorldView, compute : => T) {
	var value : T = compute
	var lastResolved : GameEventClock = worldView.currentTime
	def resolve() : T = {
		if (worldView.currentTime > lastResolved) {
			lastResolved = worldView.currentTime
			value = compute
		}
		value
	}
}
object WorldCachedValue {
	implicit def autoResolve[T](wc : WorldCachedValue[T]) : T = wc.resolve()
}

class DataModificationLog[C <: TAuxData](val baseValue : C, val finalValue : C, val fieldBreakdowns : Map[Field[_,_], Breakdown[Any]]) {
//	def breakdownFor[T](field : Field[C,T]) : Breakdown[T] = {
//		fieldBreakdowns.get(field) match {
//			case Some(breakdown) => breakdown.asInstanceOf[Breakdown[T]]
//			case None => Breakdown(field.getter(finalValue), Vector())
//		}
//	}

	def breakdownFor[T](field : Field[C,T], baseSource : String) : Breakdown[T] = {
		val baseBreakdown = fieldBreakdowns.get(field) match {
			case Some(breakdown) => breakdown.asInstanceOf[Breakdown[T]]
			case None => Breakdown(field.getter(finalValue), Vector())
		}

		val baseFieldValue = field.getter(baseValue)
		// if the base value is not conceptually empty (0, 0.0f, false, empty set, etc) then add an explicit reference to the base value
		if (!ArxGeneric.isConceptuallyEmpty(baseFieldValue)) {
			Breakdown(baseBreakdown.total, BreakdownElement[T](Some(baseSource), s"$baseFieldValue", Impact.Neutral) +: baseBreakdown.elements)
		} else {
			baseBreakdown
		}
	}
}


class EntityDataStore[T](clazz : Class[T]) {
	val values = AtomicMap.atomicNBHM[Long, EntityDataWrapper[T]]
	val overlay = AtomicMap.atomicNBHM[Long, T]
	var hasOverlay = false
	val sentinel = ReflectionAssistant.instantiate(clazz)

	def entities = values.keys.view.map(k => new LEntity(k))

	protected[engine] def putUntyped(entity : LEntity, data : AnyRef, time : GameEventClock) : Unit = {
		put(entity, data.asInstanceOf[T], time)
	}

	def put(entity : LEntity, data : T, time : GameEventClock) : Unit = {
		values.put(entity.id, new EntityDataWrapper[T](data, time))
	}

	protected[engine] def putOverlayUntyped(entity : LEntity, data : AnyRef) : Unit = {
		hasOverlay = true
		overlay.put(entity.id, data.asInstanceOf[T])
	}

	def clearOverlay(): Unit = {
		overlay.clear()
		hasOverlay = false
	}

	def getOpt(entity : LEntity) : Option[T] = {
		if (hasOverlay) {
			overlay.get(entity.id).orElse(values.get(entity.id).map(v => v.data))
		} else {
			values.get(entity.id).map(v => v.data)
		}
	}

	def get(entity : LEntity) : T = {
		if (hasOverlay) {
			overlay.get(entity.id).orElse(values.get(entity.id).map(v => v.data)).getOrElse(sentinel)
		} else {
			values.get(entity.id).map(v => v.data).getOrElse(sentinel)
		}
	}

	def getOverlayOpt(entity : LEntity) : Option[T] = {
		if (hasOverlay) {
			overlay.get(entity.id)
		} else {
			None
		}
	}

	def contains(entity : LEntity) : Boolean = {
		values.contains(entity.id) || (hasOverlay && overlay.contains(entity.id))
	}

	protected[engine] def getUntyped(entity : LEntity) : AnyRef = {
		this.get(entity).asInstanceOf[AnyRef]
	}

	def copyForEntitiesAtTime(entityIdsSet : Set[Long], atTime : GameEventClock) : this.type = {
		val newDataStore = new EntityDataStore[T](clazz)

		values.foreach { case (entityId, value) => {
			if (entityIdsSet(entityId) && value.addedAt <= atTime) {
				newDataStore.values.put(entityId, CopyAssistant.copy(value))
			}
		}}

		newDataStore.asInstanceOf[this.type]
	}
}

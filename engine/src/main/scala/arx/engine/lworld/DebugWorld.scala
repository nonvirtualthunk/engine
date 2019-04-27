package arx.engine.lworld

import arx.core.introspection.{Clazz, Field}
import arx.engine.data.TAuxData
import arx.engine.event.GameEvent

import scala.reflect.ClassTag

object DebugWorld {
	def selectFields(field : Field[_ <: AnyRef,_ <: AnyRef]*) = new WorldQuery(world.view).selectFields(field:_*)
	def selectData(clazz : Clazz[_ <: AnyRef]*) = new WorldQuery(world.view).selectData(clazz:_*)
	def selectEvents() = new WorldQuery(world.view).selectEvents()

	var world : LWorld = _
}


class WorldQuery(view : LWorldView) {
	var selectedFields : Vector[Field[_ <: AnyRef,_ <: AnyRef]] = Vector()
	var selectedData : Vector[Clazz[_ <: AnyRef]] = Vector()
	var shouldSelectEvents : Boolean = false
	var dataConditions : Vector[(LWorldView, LEntity) => Boolean] = Vector()
	var entityConditions : Vector[(LWorldView, LEntity) => Boolean] = Vector()
	var eventConditions : Vector[(LWorldView, GameEvent) => Boolean] = Vector()

	def selectFields(field : Field[_ <: AnyRef,_ <: AnyRef]*) = { selectedFields ++= field.toVector; this }
	def selectData(clazz : Clazz[_ <: AnyRef]*) = { selectedData ++= clazz.toVector; this }
	def selectEvents() = { shouldSelectEvents = true; this }

	def where[C <: TAuxData] (condition : C => Boolean)(implicit classTag : ClassTag[C]) = {
		dataConditions :+= ((v : LWorldView,e : LEntity) => v.dataOpt[C](e).exists(d => condition(d)))
		this
	}
	def where[C <: TAuxData](clazz : Clazz[C], condition : C => Boolean) = {
		dataConditions :+= ((v : LWorldView, e : LEntity) => v.dataOptByClass[C](e, clazz.runtimeClass).exists(d => condition(d)))
	}

	def whereId(id : Long) : WorldQuery = whereEntity(new LEntity(id))
	def whereEntity(target : LEntity) : WorldQuery  = {
		entityConditions :+= ((v : LWorldView,e : LEntity) => e == target)
		this
	}

	def whereEventOfType(eventType : Class[_]) = {
		eventConditions :+= ((v : LWorldView, e : GameEvent) => e.getClass == eventType)
		this
	}
	def whereEvent[C](eventType : Class[C], condition : C => Boolean) = {
		eventConditions :+= ((v : LWorldView, e : GameEvent) => {
			if (e.getClass == eventType) {
				condition(e.asInstanceOf[C])
			} else { false }
		})
		this
	}

	def whereEntityHasData(clazz : Clazz[_]) = {
		entityConditions :+= ((v : LWorldView,e : LEntity) => v.hasDataByClass(e, clazz.runtimeClass))
		this
	}

	def run() : WorldQueryResult = {

		val entityResults : Map[LEntity,Map[String,Any]] = if (shouldSelectEvents) { Map() }
		else {
			view.entities.filter(e => entityConditions.forall(cond => cond(view,e.entity)))
				.map(e => {
					val fieldResults = selectedFields.map(f => {
						val key = s"${f.name}"
						val untypedData = view.dataOptByClass(e.entity, f.clazz.runtimeClass)
						key -> untypedData.map(d => f.getValue(d)).getOrElse(None)
					}).toMap

					val dataResults = selectedData.map(d => {
						val key = s"${d.className}"
						key -> view.dataOptByClass(e.entity, d.runtimeClass).getOrElse(None)
					})

					e.entity -> (fieldResults ++ dataResults)
				}).toMap
		}

		val eventResults = if (!shouldSelectEvents) { Vector() }
		else {
			view.events.filter(e => eventConditions.forall(condition => condition(view, e.event))).map(e => e.event)
		}


		WorldQueryResult(entityResults, eventResults)
	}
}

case class WorldQueryResult(entityResults : Map[LEntity, Map[String, Any]], eventResults : Vector[GameEvent]) {
	override def toString: String = {
		val stringBuilder = new StringBuilder
		entityResults.foreach {
			case (entity, results) => {
				if (stringBuilder.nonEmpty && stringBuilder.last != '\n') {stringBuilder.append("\n")}
				stringBuilder.append(s"$entity:\n")
				results.foreach {
					case (identifier, result) => {
						if (stringBuilder.nonEmpty && stringBuilder.last != '\n') {stringBuilder.append("\n")}
						val resultStr = result.toString
						stringBuilder.append(s"\t$identifier :")
						val adjustedResultStr = if (resultStr.contains('\n')) {
							"\n\t\t" + resultStr.replaceAll("\n","\n\t\t")
						} else {
							" " + resultStr
						}

						stringBuilder.append(adjustedResultStr)
					}
				}
			}
		}

		eventResults.foreach(e => {
			stringBuilder.append("- ").append(e.toString).append("\n")
		})

		stringBuilder.mkString
	}
}

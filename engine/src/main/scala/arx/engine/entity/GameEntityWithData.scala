package arx.engine.entity

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 12/23/15
  * Time: 8:22 AM
  */

import arx.Prelude._
import arx.engine.data.TAuxData
import arx.engine.data.TGameEntityAuxData
import arx.engine.data.THasAuxData
import scala.language.implicitConversions
import scalaxy.loops._
import arx.core.vec._

class GameEntityWithData[T <: TGameEntityAuxData](protected val intern : TGameEntity, clazz : Class[T]) {
	def this(ent : TGameEntity)(implicit ev : Manifest[T]) {
		this(ent, ev.runtimeClass.asInstanceOf[Class[T]])
	}

	@transient protected var cached = intern.auxData(clazz)

	def getAuxData = {
		if (cached == null) {
			cached = intern.auxData(clazz)
		}
		cached
	}
}
object GameEntityWithData {
	implicit def fromEntity[T <: TGameEntityAuxData : Manifest](e : TGameEntity) : GameEntityWithData[T] = {
		new GameEntityWithData[T](e)
	}

	implicit def toEntity(e : GameEntityWithData[_]) : TGameEntity = {
		e.intern
	}

	implicit def toAuxData[T <: TGameEntityAuxData](e : GameEntityWithData[T]) : T = {
		e.getAuxData
	}
}


class GameEntityWithData2[T <: TGameEntityAuxData,U <: TGameEntityAuxData](protected val intern : TGameEntity, clazz : Class[T], clazz2 : Class[U]) {
	@transient protected var cachedT = intern.auxData(clazz)
	@transient protected var cachedU = intern.auxData(clazz2)

	def getAuxDataT = {
		if (cachedT == null) {
			cachedT = intern.auxData(clazz)
		}
		cachedT
	}

	def getAuxDataU = {
		if (cachedU == null) {
			cachedU = intern.auxData(clazz2)
		}
		cachedU
	}
}
object GameEntityWithData2 {
	implicit def fromEntity[T <: TGameEntityAuxData, U <: TGameEntityAuxData](e : TGameEntity, cl1 : Class[T], cl2 : Class[U]) : GameEntityWithData2[T,U] = {
		new GameEntityWithData2[T,U](e, cl1, cl2)
	}

	implicit def toEntity(e : GameEntityWithData2[_,_]) : TGameEntity = {
		e.intern
	}

	implicit def toAuxDataT[T <: TGameEntityAuxData,U<: TGameEntityAuxData](e : GameEntityWithData2[T,U]) : T = {
		e.getAuxDataT
	}

	implicit def toAuxDataU[T <: TGameEntityAuxData,U<: TGameEntityAuxData](e : GameEntityWithData2[T,U]) : U = {
		e.getAuxDataU
	}
}




package arx.engine.data

import arx.core.introspection.ReflectionAssistant

import scala.collection.mutable
import scala.language.implicitConversions

trait THasAuxData[U <: TAuxData] {
	def auxDataByClass[T <: U](clazz: Class[T]): T

	def auxData[T <: U : Manifest]: T = auxDataByClass(manifest[T].runtimeClass.asInstanceOf[Class[T]])

	protected def onNewAuxDataCreated(gead: U) {}

	def auxDataOrElse[T <: U : Manifest](orElse: T): T

	def allAuxData: List[U]

	/** Returns an auxData that descends from the given type, if one exists */
	def auxDataWithTrait[T: Manifest] = allAuxData.find(v => manifest[T].runtimeClass.isAssignableFrom(v.getClass)).asInstanceOf[Option[T]]

	def removeAuxData[T <: U : Manifest]() { removeAuxData(manifest[T].runtimeClass.asInstanceOf[Class[T]]) }

	def removeAuxData[T <: U](clazz: Class[T])

	def manualAddAuxData(d: U): Unit = {
		addAuxData(d)
	}

	protected def addAuxData(d: U): Unit = {
		onNewAuxDataCreated(d)
		d.onAssignedToObject(this)
		storeAuxData(d)
	}

	protected[engine] def storeAuxData(d: U)

	def aux[T <: U : Manifest] = auxData[T]

	def apply[T <: U : Manifest] = auxData[T]

	def auxDataOpt[T <: U : Manifest]: Option[T]

	final def hasAuxData[T <: U : Manifest]: Boolean = hasAuxDataByClass(manifest[T].runtimeClass.asInstanceOf[Class[T]])

	def hasAuxDataByClass[T <: U](clazz : Class[T]) : Boolean

	def withData[R <: U : Manifest] : WrappedWithData[U,R,THasAuxData[U]] = new WrappedWithData[U, R, THasAuxData[U]](this)

	protected def auxByName(str : String) = {
		allAuxData.filter(a => a.getClass.getSimpleName.endsWith(str)).head
	}
}


class WrappedWithData[BaseT <: TAuxData, T <: BaseT : Manifest, +E <: THasAuxData[BaseT]](protected val intern: E) extends THasAuxData[BaseT] {
	@transient protected var cached = intern.auxDataByClass(manifest[T].runtimeClass.asInstanceOf[Class[T]])

	def getAuxData = {
		if (cached == null) {
			cached = intern.auxDataByClass(manifest[T].runtimeClass.asInstanceOf[Class[T]])
		}
		cached
	}

	override def withData[U <: BaseT : Manifest] : WrappedWithData2[BaseT,U,WrappedWithData[BaseT,T,E]] = new WrappedWithData2[BaseT,U,WrappedWithData[BaseT,T,E]](this)

	override def auxDataByClass[U <: BaseT](clazz: Class[U]): U = intern.auxDataByClass[U](clazz)

	override protected[engine] def storeAuxData(d: BaseT): Unit = intern.storeAuxData(d)

	override def hasAuxDataByClass[U <: BaseT](clazz : Class[U]) = intern.hasAuxDataByClass(clazz)

	override def removeAuxData[U <: BaseT](clazz: Class[U]): Unit = intern.removeAuxData(clazz)

	override def auxDataOpt[U <: BaseT : Manifest]: Option[U] = intern.auxDataOpt[U]

	override def allAuxData: List[BaseT] = allAuxData

	override def auxDataOrElse[U <: BaseT : Manifest](orElse: U): U = auxDataOrElse[U](orElse)

	override def apply[U <: BaseT : Manifest] = intern.auxData[U]

	override def equals(obj: scala.Any): Boolean = {
		obj match {
			case e : E => intern == e
			case w : WrappedWithData[_,_,E] => w.intern == this.intern
			case _ => super.equals(obj)
		}
	}
}

class WrappedWithData2[BaseT <: TAuxData, T <: BaseT : Manifest, +E <: THasAuxData[BaseT]](intern: E) extends WrappedWithData[BaseT,T,E](intern) {
	override def withData[U <: BaseT : Manifest] : WrappedWithData3[BaseT,U,WrappedWithData2[BaseT,T,E]] =
		new WrappedWithData3[BaseT,U,WrappedWithData2[BaseT,T,E]](this)
}

class WrappedWithData3[BaseT <: TAuxData, T <: BaseT : Manifest, +E <: THasAuxData[BaseT]](intern: E) extends WrappedWithData2[BaseT,T,E](intern) {

}

object WrappedWithData {
	implicit def unwrap1[BaseT <: TAuxData, T <: BaseT, E <: THasAuxData[BaseT]](wrapped: WrappedWithData[BaseT, T, E]): E = {
		wrapped.intern
	}

	implicit def unwrap2[BaseT <: TAuxData, E <: THasAuxData[BaseT]](wrapped: WrappedWithData2[BaseT, _, WrappedWithData[BaseT,_,E]]): E = {
		wrapped.intern.intern
	}

	implicit def unwrap3[BaseT <: TAuxData, E <: THasAuxData[BaseT]](wrapped: WrappedWithData3[BaseT, _, WrappedWithData2[BaseT,_,WrappedWithData[BaseT,_,E]]]): E = {
		wrapped.intern.intern.intern
	}

	implicit def downwrapA[BaseT <: TAuxData, E <: THasAuxData[BaseT]](wrapped: WrappedWithData2[BaseT, _, WrappedWithData[BaseT,_,E]]): WrappedWithData[BaseT,_,E] = {
		wrapped.intern
	}

	implicit def downwrapB[BaseT <: TAuxData, T <: BaseT : Manifest, E <: THasAuxData[BaseT]](wrapped: WrappedWithData2[BaseT, T, WrappedWithData[BaseT,_,E]]): WrappedWithData[BaseT,T,E] = {
		new WrappedWithData[BaseT,T,E](wrapped.intern.intern)
	}

	implicit def getData[BaseT <: TAuxData, T <: BaseT, _](wrapped: WrappedWithData[BaseT, T, _]): T = {
		wrapped.getAuxData
	}

	implicit def getData2[BaseT <: TAuxData, T <: BaseT](wrapped: WrappedWithData2[BaseT, _, WrappedWithData[BaseT, T, _]]): T = {
		wrapped.intern.getAuxData
	}

	implicit def getData3[BaseT <: TAuxData, T <: BaseT](wrapped: WrappedWithData3[BaseT, _, WrappedWithData2[BaseT, _, WrappedWithData[BaseT, T, _]]]): T = {
		wrapped.intern.intern.getAuxData
	}

	implicit def wrap[BaseT <: TAuxData, T <: BaseT : Manifest, E <: THasAuxData[BaseT]](toWrap: E): WrappedWithData[BaseT, T, E] = {
		new WrappedWithData[BaseT, T, E](toWrap)
	}
}

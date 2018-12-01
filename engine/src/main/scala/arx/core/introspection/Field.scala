package arx.core.introspection

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/27/18
  * Time: 6:51 AM
  */

import arx.Prelude._
import arx.core.async.Executor
import arx.core.macros.GenerateCompanion
import scalaxy.loops._
import arx.core.vec._
import arx.engine.data.TAuxData

trait UntypedField {
	def getValue (objectReference : AnyRef) : Any
	def setValue (objectReference : AnyRef, value : Any)
	def name : String
}

case class Field[C,T](name : String, getter : (C) => T, setter : (C,T) => Unit) extends UntypedField {


	override def getValue(objectReference: AnyRef): Any = {
		objectReference match {
			case ref : C => getter(ref)
			case other => throw new IllegalStateException("Untyped getValue(...) on field violated type constraints at runtime")
		}
	}

	override def setValue(objectReference: AnyRef, value: Any): Unit = {
		objectReference match {
			case ref : C => {
				value match {
					case validValue : T => setter(ref, validValue)
					case other => throw new IllegalStateException("Untyped setValue(...) on field violated type constraints of value at runtime")
				}
			}
			case other => throw new IllegalStateException("Untyped setValue(...) on field violated type constraints at runtime")
		}
	}
}

//class FieldBuilder[C](name : String) {
//	def withAccessors[T](getter : (C) => T, setter : (C,T) => Unit) : Field[C,T] = Field(name,getter,setter)
//}
//
//object Field {
//	def createFromExample[C,T](name : String, getter : (C) => T, setter : (C,T) => Unit, classValue : C, value : T) : Field[C,T] = {
//		Field(name,getter,setter)
//	}
////	def apply[C](name : String) : FieldBuilder[C] = new FieldBuilder[C](name)
//}

/**
  * Workaround for the limitations of reflection and type inference. Because the actual generic type is not known at
  * runtime we can't rely on it when generating the code, or we just end up with Option[_] instead of Option[String].
  * But, if we create a sentinel instance in the companion object, and make use of references to the concrete values
  * it contains, we can convince scala's type inference to figure out which type things should be
  */
class FieldExample[T](value : T) {
	def createField[C](name : String, getter : (C) => T, setter : (C,T) => Unit) : Field[C,T] = {
		Field[C,T](name,getter,setter)
	}
}
object Field {
	def fromValue[T](value : T) : FieldExample[T] = new FieldExample(value)
}


object FieldGenerator {
	def main(args: Array[String]): Unit = {
		for (clazz <- ReflectionAssistant.allSubTypesOf[TAuxData]) {
			if (clazz.getAnnotations.exists(a => a.annotationType() == classOf[GenerateCompanion])) {
				val className = clazz.getSimpleName
				println(s"object $className {")
				println(s"\tval Sentinel = new $className")
				for (field <- clazz.getDeclaredFields) {
					val fieldName = field.getName

					println(s"""\tval $fieldName = Field.fromValue(Sentinel.$fieldName).createField[$className]("$fieldName",f => f.$fieldName, (f,$fieldName) => f.$fieldName = $fieldName) """)
				}
				println("}")
			}
		}

		Executor.onQuit()
	}
}
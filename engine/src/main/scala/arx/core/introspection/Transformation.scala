package arx.core.introspection

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/27/18
  * Time: 8:02 AM
  */

import arx.Prelude._
import arx.engine.lworld.FieldOperationModifier
import scalaxy.loops._

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag


trait Transformation[T] {
	def apply(oldValue : T) : T = transform(oldValue)

	/** Immutable operation returning a new value based on the old. Must not mutate given value */
	def transform(oldValue: T): T

	def asSimpleString: String
}


object FieldOperations {

	case class Set[T](value: T) extends Transformation[T] {
		override def transform(oldValue: T): T = {
			value
		}

		override def asSimpleString: String = s"= $value"
	}

	case class Add[T: Numeric](value: T) extends Transformation[T] {
		override def transform(oldValue: T): T = {
			implicitly[Numeric[T]].plus(oldValue, value)
		}

		override def asSimpleString: String = implicitly[Numeric[T]].signum(value) match {
			case -1 => s"- $value"
			case _ => s"+ $value"
		}
	}

	case class Sub[T: Numeric](value: T) extends Transformation[T] {
		override def transform(oldValue: T): T = {
			implicitly[Numeric[T]].minus(oldValue, value)
		}

		override def asSimpleString: String = implicitly[Numeric[T]].signum(value) match {
			case -1 => s"+ $value"
			case _ => s"- $value"
		}

	}

	case class Mul[T: Numeric](value: T) extends Transformation[T] {
		override def transform(oldValue: T): T = {
			implicitly[Numeric[T]].times(oldValue, value)
		}

		override def asSimpleString: String = s"* $value"
	}

	case class Div[T: Integral](divisor: T) extends Transformation[T] {
		override def transform(oldValue: T): T = {
			implicitly[Integral[T]].quot(oldValue, divisor)
		}

		override def asSimpleString: String = s"/ $divisor"
	}

	case class Append[U](value : U) extends Transformation[Seq[U]] {
		override def transform(oldValue: Seq[U]): Seq[U] = {
			oldValue :+ value
		}

		override def asSimpleString: String = s":+ $value"
	}

	case class SetKey[K,V](entry : (K,V)) extends Transformation[Map[K,V]] {
		/** Immutable operation returning a new value based on the old. Must not mutate given value */
		override def transform(oldValue: Map[K, V]): Map[K, V] = {
			oldValue + entry
		}

		override def asSimpleString: String = s"${entry._1} -> ${entry._2}"
	}

	case class RemoveKey[K,V](key : K) extends Transformation[Map[K,V]] {
		/** Immutable operation returning a new value based on the old. Must not mutate given value */
		override def transform(oldValue: Map[K, V]): Map[K, V] = {
			oldValue - key
		}

		override def asSimpleString: String = s"- $key"
	}

	implicit class NumericField[C, T: Numeric](field: Field[C, T])(implicit val tag : ClassTag[C]) {
		def +(value: T) = FieldOperationModifier(field, Add(value))

		def -(value: T) = FieldOperationModifier(field, Sub(value))

		def *(value: T) = FieldOperationModifier(field, Mul(value))
	}

	implicit class IntegralField[C, T: Integral](field: Field[C, T])(implicit val tag : ClassTag[C]) {
		def /(divisor: T) = FieldOperationModifier(field, Div(divisor))
	}

	implicit class SettableField[C,T](field : Field[C,T])(implicit val tag : ClassTag[C]) {
		def -> (value : T) = FieldOperationModifier(field, Set(value))
	}

	implicit class SeqField[C,U](field : Field[C,Seq[U]])(implicit val tag : ClassTag[C]) {
		def append(elem : U) = FieldOperationModifier(field, Append(elem))
	}

	implicit class MapField[C,K,V](field : Field[C,Map[K,V]])(implicit val tag : ClassTag[C]) {
		def put(key : K, value : V) = FieldOperationModifier(field, SetKey((key,value)))
		def +(entry : (K,V)) = FieldOperationModifier(field, SetKey(entry))
		def remove(key : K) = FieldOperationModifier[C,Map[K,V]](field, RemoveKey(key))
	}

//	implicit class CollectionField[C,U,T : Seq[U]](field : Field[C,T]) {
//		def append (value : U) : Append[U,T] = Append()
//	}
}


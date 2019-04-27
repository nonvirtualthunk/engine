package arx.core.introspection

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/27/18
  * Time: 8:02 AM
  */

import arx.Prelude._
import arx.engine.data.Reduceable
import arx.engine.lworld.{Impact, FieldOperationModifier}
import scalaxy.loops._

import scala.collection
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag


trait Transformation[T] {
	def apply(oldValue : T) : T = transform(oldValue)

	/** Immutable operation returning a new value based on the old. Must not mutate given value */
	def transform(oldValue: T): T

	def asSimpleString: String

	def impact : Impact
}


object FieldOperations {

	case class SetTo[T](value: T) extends Transformation[T] {
		override def transform(oldValue: T): T = {
			value
		}

		override def asSimpleString: String = s"= $value"
		override def impact = Impact.Neutral
	}

	case class Add[T: Numeric](value: T) extends Transformation[T] {
		override def transform(oldValue: T): T = {
			implicitly[Numeric[T]].plus(oldValue, value)
		}

		override def asSimpleString: String = implicitly[Numeric[T]].signum(value) match {
			case -1 => s"- $value"
			case _ => s"+ $value"
		}

		override def impact = implicitly[Numeric[T]].compare(value, implicitly[Numeric[T]].zero) match {
			case i if i > 0 => Impact.Positive
			case i if i < 0 => Impact.Negative
			case i if i == 0 => Impact.Neutral
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

		override def impact = implicitly[Numeric[T]].compare(value, implicitly[Numeric[T]].zero) match {
			case i if i < 0 => Impact.Positive
			case i if i > 0 => Impact.Negative
			case i if i == 0 => Impact.Neutral
		}
	}

	case class Mul[T: Numeric](value: T) extends Transformation[T] {
		override def transform(oldValue: T): T = {
			implicitly[Numeric[T]].times(oldValue, value)
		}

		override def asSimpleString: String = s"* $value"

		override def impact = implicitly[Numeric[T]].compare(value, implicitly[Numeric[T]].one) match {
			case i if i > 0 => Impact.Positive
			case i if i < 0 => Impact.Negative
			case i if i == 0 => Impact.Neutral
		}
	}

	case class Div[T: Integral](divisor: T) extends Transformation[T] {
		override def transform(oldValue: T): T = {
			implicitly[Integral[T]].quot(oldValue, divisor)
		}

		override def asSimpleString: String = s"/ $divisor"

		override def impact = implicitly[Numeric[T]].compare(divisor, implicitly[Numeric[T]].one) match {
			case i if i < 0 => Impact.Positive
			case i if i > 0 => Impact.Negative
			case i if i == 0 => Impact.Neutral
		}
	}

	case class ReduceBy[T : Numeric](value: T, limitToZero : Boolean) extends Transformation[Reduceable[T]] {
		override def transform(oldValue: Reduceable[T]): Reduceable[T] = {
			oldValue.reduceBy(value, limitToZero)
		}

		override def asSimpleString: String = s"reduce by $value"

		override def impact = implicitly[Numeric[T]].compare(value, implicitly[Numeric[T]].zero) match {
			case i if i < 0 => Impact.Positive
			case i if i > 0 => Impact.Negative
			case i if i == 0 => Impact.Neutral
		}
	}

	case class RecoverBy[T : Numeric](value: T) extends Transformation[Reduceable[T]] {
		override def transform(oldValue: Reduceable[T]): Reduceable[T] = {
			oldValue.recoverBy(value, limitToZero = true)
		}

		override def asSimpleString: String = s"recover by $value"

		override def impact = implicitly[Numeric[T]].compare(value, implicitly[Numeric[T]].zero) match {
			case i if i > 0 => Impact.Positive
			case i if i < 0 => Impact.Negative
			case i if i == 0 => Impact.Neutral
		}
	}

	class RecoverToFull[T : Numeric] extends Transformation[Reduceable[T]] {
		override def transform(oldValue: Reduceable[T]): Reduceable[T] = {
			oldValue.recoverToFull()
		}

		override def asSimpleString: String = s"recovered to full"

		override def impact = Impact.Positive
	}

	case class Append[U](value : U) extends Transformation[Seq[U]] {
		override def transform(oldValue: Seq[U]): Seq[U] = {
			oldValue :+ value
		}

		override def asSimpleString: String = s":+ $value"

		override def impact = Impact.Positive
	}

	case class SetKey[K,V](entry : (K,V)) extends Transformation[Map[K,V]] {
		/** Immutable operation returning a new value based on the old. Must not mutate given value */
		override def transform(oldValue: Map[K, V]): Map[K, V] = {
			oldValue + entry
		}

		override def asSimpleString: String = s"${entry._1} -> ${entry._2}"

		override def impact = Impact.Neutral
	}

	case class RemoveKey[K,V](key : K) extends Transformation[Map[K,V]] {
		/** Immutable operation returning a new value based on the old. Must not mutate given value */
		override def transform(oldValue: Map[K, V]): Map[K, V] = {
			oldValue - key
		}

		override def asSimpleString: String = s"- $key"

		override def impact = Impact.Negative
	}

	case class InsertValue[V](value : V) extends Transformation[collection.immutable.Set[V]] {
		/** Immutable operation returning a new value based on the old. Must not mutate given value */
		override def transform(oldValue: collection.immutable.Set[V]): collection.immutable.Set[V] = oldValue + value
		override def asSimpleString: String = s"+ $value"

		override def impact = Impact.Positive
	}

	case class RemoveValue[V](value : V) extends Transformation[collection.immutable.Set[V]] {
		/** Immutable operation returning a new value based on the old. Must not mutate given value */
		override def transform(oldValue: collection.immutable.Set[V]): collection.immutable.Set[V] = oldValue - value
		override def asSimpleString: String = s"+ $value"

		override def impact = Impact.Negative
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
		def -> (value : T) = FieldOperationModifier(field, SetTo(value))
		def setTo (value : T) = FieldOperationModifier(field, SetTo(value))
	}

	implicit class SeqField[C,U](field : Field[C,Seq[U]])(implicit val tag : ClassTag[C]) {
		def append(elem : U) = FieldOperationModifier(field, Append(elem))
	}

	implicit class MapField[C,K,V](field : Field[C,Map[K,V]])(implicit val tag : ClassTag[C]) {
		def put(key : K, value : V) = FieldOperationModifier(field, SetKey((key,value)))
		def +(entry : (K,V)) = FieldOperationModifier(field, SetKey(entry))
		def remove(key : K) = FieldOperationModifier[C,Map[K,V]](field, RemoveKey(key))
	}

	implicit class SetField[C,V](field : Field[C,collection.immutable.Set[V]])(implicit val tag : ClassTag[C]) {
		def + (value : V) = FieldOperationModifier(field, InsertValue(value))
		def - (value : V) = FieldOperationModifier(field, RemoveValue(value))
	}

	implicit class ReduceableField[C, T : Numeric](field: Field[C, Reduceable[T]])(implicit val tag : ClassTag[C]) {
		def reduceBy(value: T) = FieldOperationModifier(field, ReduceBy(value, limitToZero = true))

		def reduceBy(value: T, limitToZero : Boolean) = FieldOperationModifier(field, ReduceBy(value, limitToZero))

		def recoverBy(value: T) = FieldOperationModifier(field, RecoverBy(value))

		def recoverToFull() = FieldOperationModifier(field, new RecoverToFull[T]())
	}


	//	implicit class CollectionField[C,U,T : Seq[U]](field : Field[C,T]) {
//		def append (value : U) : Append[U,T] = Append()
//	}
}


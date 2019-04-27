package arx.core.traits

import arx.core.math.FixedFractional

// experimental, consider removing if we don't use
///**
//  * Wrapper class
//  */
object ArxGeneric {


	def isConceptuallyEmpty(a : Any) : Boolean = {
		a match {
			case i : Int => i == 0
			case s : Short => s == 0
			case b : Byte => b == 0
			case l : Long => l == 0
			case f : Float => f == 0.0f
			case d : Double => d == 0.0f
			case b : Boolean => !b
			case f : FixedFractional[_] => f.asInt == 0
			case s : Traversable[_] => s.isEmpty
			case o : Option[_] => o.isEmpty
			case null => true
			case _ => false
		}
	}

//
//	def combine[T](a : T, b : T) = {
//		a match {
//			case i : Int => (b.asInstanceOf[Int] + i).asInstanceOf[T]
//			case i : Short => (b.asInstanceOf[Short] + i).asInstanceOf[T]
//			case i : Double => (b.asInstanceOf[Double] + i).asInstanceOf[T]
//			case i : Float => (b.asInstanceOf[Float] + i).asInstanceOf[T]
//			case i : Long => (b.asInstanceOf[Long] + i).asInstanceOf[T]
//			case i : FixedFractional[_] => (b.asInstanceOf[FixedFractional[_]] + i).asInstanceOf[T]
//			case a : Set[_] =>
//		}
//	}
}
//
//trait CombineableType[T] {
//	def combine(a : T, b : T) : T
//}
//
//object CombineableType {
//	val combineFloat = new CombineableType[Float] {
//		override def combine(a: Float, b: Float): Float = a + b
//	}
//	val combineDouble = new CombineableType[Int] {
//		override def combine(a : Int, b : Int) : Int = a + b
//	}
//	val combineSets = new CombineableType[Set[_]] {
//		override def combine(a: Set[_], b: Set[_]): Set[_] = a ++ b
//	}
//}
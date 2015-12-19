package arx.core.function

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 10:20 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.core.CachedValue

import scala.collection.mutable
import scala.collection.mutable.HashMap

class MemoizingFunction[T,U] ( func : (T) => U ) extends Function1[T,U] with Serializable {
	val cache = new mutable.HashMap[T,U]()
	def apply(t: T): U = {
		cache.getOrElseUpdate(t,func(t))
	}
}

class MemoizingFunction2[S,T,U] ( func : (S,T) => U ) extends Function2[S,T,U] with Serializable {
	val cache = new HashMap[(S,T),U]()
	def apply(s: S,t: T): U = {
		cache.getOrElseUpdate((s,t),func(s,t))
	}
}

class MemoizingFunction3[R,S,T,U] ( func : (R,S,T) => U ) extends Function3[R,S,T,U] with Serializable {
	val cache = new HashMap[(R,S,T),U]()
	def apply(r: R,s: S,t: T): U = {
		cache.getOrElseUpdate((r,s,t),func(r,s,t))
	}
}

class MemoizingFunction4[Q,R,S,T,U] ( func : (Q,R,S,T) => U ) extends Function4[Q,R,S,T,U] with Serializable {
	val cache = new HashMap[(Q,R,S,T),U]()
	def apply(q : Q,r: R,s: S,t: T): U = {
		cache.getOrElseUpdate((q,r,s,t),func(q,r,s,t))
	}
}

class MemoizingCachedFunction1[T <: AnyRef,U <: AnyRef] ( func : (T) => U ) extends Function1[T,U] with Serializable {
	val cache = new mutable.HashMap[T,CachedValue[U]]
	def apply(t: T) : U = {
		val cv = cache.getOrElseUpdate(t,new CachedValue(func(t)))
		cv.resolve()
	}
}

class SingleMemoizingFunction[T,U] ( func : (T) => U ) extends Function1[T,U] with Serializable {
	var cache : Option[(T,U)] = None
	def apply ( t : T ) = {
		cache = cache match {
			case Some((lastT,lastU)) if t == lastT => cache
			case _ => Some(t -> func(t))
		}
		cache.get._2
	}
}

class SingleMemoizingFunction2[S,T,U] ( func : (S,T) => U ) extends Function2[S,T,U] with Serializable {
	var cache : Option[(S,T,U)] = None
	def apply ( s : S, t : T ) = {
		cache = cache match {
			case Some((lastS,lastT,lastU)) if t == lastT && s == lastS => cache
			case _ => Some((s, t , func(s,t)))
		}
		cache.get._3
	}
}
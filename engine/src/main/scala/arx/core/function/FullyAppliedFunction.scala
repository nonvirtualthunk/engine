package arx.core.function

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 10:23 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.Moddable

class FullyAppliedFunction[-T,+R](function : (T) => R, arg : T) extends (() => R) with Serializable with Moddable[R] {
	def apply() = function(arg)

	def resolve() = apply()
	def baseValue() = apply()
	override def dynamic = true
}
class FullyAppliedFunction2[-T,-U,+R](function : (T,U) => R, arg1 : T, arg2 : U) extends (() => R) with Serializable with Moddable[R] {
	def apply() = function(arg1,arg2)

	def resolve() = apply()
	def baseValue() = apply()
	override def dynamic = true
}

object FullyAppliedFunction {
	def apply[T,R](f : (T) => R, arg : T) = new FullyAppliedFunction[T,R](f,arg)
	def apply[T,U,R](f : (T,U) => R, arg : T,arg2 : U) = new FullyAppliedFunction2[T,U,R](f,arg,arg2)
}
object FAF {
	def apply[T,R](f : (T) => R, arg : T) = new FullyAppliedFunction[T,R](f,arg)
	def apply[T,U,R](f : (T,U) => R, arg : T,arg2 : U) = new FullyAppliedFunction2[T,U,R](f,arg,arg2)
}
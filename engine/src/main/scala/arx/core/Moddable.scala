package arx.core

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/4/12
 * Time: 11:01 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import java.util.concurrent.atomic.AtomicBoolean

import scala.annotation.tailrec

@SerialVersionUID(1L)
trait Moddable[+T] extends Serializable {
	def resolve(): T

	def dynamic: Boolean = true

	def baseValue(): T

	override def equals(p1: Any) = p1 match {
		case null => false
		case m: Moddable[_] => m.resolve() == this.resolve()
		case o => this.resolve() == o
	}

	def toFunction = new FunctionFromModdable(this)
}

@Deprecated
trait SelfModdable[+T] extends Moddable[T] {
	override def equals(p1: Any) = p1 match {
		case null => false
		case s: SelfModdable[T] => super.equals(p1)
		case m: Moddable[T] => this.equals(m.resolve())
		case o => this.resolve() == o
	}
}

class FunctionFromModdable[+T](moddable: Moddable[T]) extends Function0[T] {
	def apply() = moddable.resolve()
}

class ModdedValue[T](val value: T) extends Moddable[T] {
	override def dynamic: Boolean = false

	override def resolve(): T = value

	override def baseValue() = value

	override def toString: String = { resolve().toString }

	override def equals(p1: Any) = p1 match {
		case null => false
		case m: Moddable[T] => m.resolve() == this.resolve()
		case o => this.resolve() == o
	}
}

abstract class WrappingModdable[T](var modded: Moddable[T]) extends Moddable[T] {
	override def toString: String = { resolve().toString }
}

class ModdedInOutFunction[T](m: Moddable[T], val func: T => T) extends WrappingModdable[T](m) {
	var active: Boolean = true
	@transient var resolving: Boolean = false

	override def resolve(): T = {
		synchronized {
			if (!resolving) {
				resolving = true
				val ret = if (active) {
					val input = modded.resolve()
					val output = func(input)
					posit(!input.isInstanceOf[AnyRef] || !output.isInstanceOf[AnyRef] || (!(input.asInstanceOf[AnyRef] eq output.asInstanceOf[AnyRef])), "modifier function had the same input as output, possible violation of immutability")
					output
				} else {
					modded.resolve()
				}
				resolving = false
				ret
			} else {
				modded.resolve()
			}
		}
	}

	def baseValue() = m.baseValue()
}

class ModdedSimpleClosure[T, U](f: (U) => T, u: Moddable[U]) extends Moddable[T] {
	def resolve() = f(u.resolve())

	def baseValue() = f(u.resolve())
}

trait Modifier[T] extends WrappingModdable[T]

class FunctionModifier[T](modded: Moddable[T], f: T => T) extends ModdedInOutFunction[T](modded, f) with Modifier[T] {

}

class ModdedOutFunction[T](val func: () => T) extends Moddable[T] {
	//	@transient var resolving  = false

	override def resolve(): T = {
		//		if ( ! resolving ) {
		//			resolving = true
		val ret = func()
		//			resolving = false
		ret
		//		} else {
		//			throw new IllegalStateException("Infinite loop detected in moddedOutFunction()")
		//		}
	}

	def baseValue() = func()
}

//class ModdedByNameFunction[T](func: => T) extends Moddable[T]{
//	override def resolve () : T = {
//		func
//	}
//}

class DamageModifier(modded: Moddable[Float], val damage: Float) extends WrappingModdable[Float](modded) with Modifier[Float] {
	def resolve(): Float = {
		modded.resolve() - damage
	}

	def baseValue() = modded.baseValue()

	def copyWithNewDamage(d: Float) = new DamageModifier(modded, d)
}


class ForwardingModdable[T](moddable: () => Moddable[T]) extends Moddable[T] {
	def resolve() = moddable().resolve()

	def baseValue() = moddable().resolve()
}

class MutableModdable(initialValue: Float) extends Moddable[Float] {
	var intern = Moddable(initialValue)

	def resolve(): Float = intern.resolve()

	def baseValue(): Float = intern.baseValue()

	def mod(func: (Float) => Float) { intern = Mod(intern, func) }

	def increase(f: Float) { intern = Mod(intern, (base: Float) => base + f) }

	def reset() { intern = Moddable(intern.baseValue()) }
}

object Mod {
	def apply[T] ( modded: Moddable[T], func: (T) => T ): Moddable[T] = { new ModdedInOutFunction[T](modded,func) }
	def apply[T,U] ( func : (U) => T , u : Moddable[U] ) : Moddable[T] = { new ModdedSimpleClosure[T,U](func,u) }
}

object Moddable{
	protected val moddable0 = new ModdedValue(0.0f)
	protected val moddable1 = new ModdedValue(1.0f)
	protected val moddable0i = new ModdedValue[Int](0)
	protected val moddable1i = new ModdedValue[Int](1)
	protected val moddableNil = new ModdedValue(Nil)

	def removeAllReferencesTo[T](to: T, from: Moddable[List[T]]) : Moddable[List[T]] = {
		from match {
			case wm : WrappingModdable[List[T]] =>
				wm.modded = removeAllReferencesTo(to,wm.modded)
				wm
			case vm : ModdedValue[List[T]] => Moddable( vm.value filterNot ( _ == to ) )
			case o : Moddable[List[T]] => o
		}
	}

	def apply ( value : Float ) : Moddable[Float] = {
		if ( value == 0.0f ) { moddable0 }
		else if ( value == 1.0f ) { moddable1 }
		else { new ModdedValue(value) }
	}
	def apply ( value : Int ) : Moddable[Int] = {
		if ( value == 0 ) { moddable0i }
		else if ( value == 1 ) { moddable1i }
		else { new ModdedValue(value) }
	}
	def apply[T] ( nil : Nil.type ): Moddable[Nil.type] = { moddableNil }
	def apply[T] ( value: T ): Moddable[T] = { new ModdedValue[T](value) }
	def apply[T] ( func: () => T ): Moddable[T] = { new ModdedOutFunction[T](func) }

	def removeDamageModifiers ( m : Moddable[Float] ) : Moddable[Float] = {
		m match {
			case dm : DamageModifier => removeDamageModifiers(dm.modded)
			case wm : WrappingModdable[Float] =>
				wm.modded = removeDamageModifiers(wm.modded)
				wm
			case om : Moddable[Float] => om
		}
	}

	/** Should replace the closest to top hard value with itself plus 'f' */
	def addToValue ( m : Moddable[Float], f : Float ) : Moddable[Float] = {
		m match {
			case wm : WrappingModdable[Float] =>
				wm.modded = addToValue(wm,f)
				wm
			case mv : ModdedValue[Float] =>
				Moddable(mv.value + f)
			case o : Moddable[Float] =>
				Noto.warn("Attempting addToValue() on a moddable backed by a non-wrapping, non-value: " + o)
				Noto.warn("Stack trace:\n")
				Noto.warn((new Exception).getStackTraceString)
				o
		}
	}

	@tailrec
	def multValue ( m : Moddable[Float], f : Float ) : Moddable[Float] = {
		m match {
			case wm : WrappingModdable[Float] =>
				wm.modded match {
					case mv : ModdedValue[Float] =>
						wm.modded = new ModdedValue(mv.value * f)
						wm
					case other : Moddable[Float] =>
						multValue(other,f)
				}
			//					wm.modded = multValue(wm,f)
			//					wm
			case mv : ModdedValue[Float] =>
				new ModdedValue(mv.value * f)
			case o : Moddable[Float] =>
				Noto.warn("Attempting multValue() on a moddable backed by a non-wrapping, non-value: " + o)
				Noto.warn("Stack trace:\n")
				Noto.warn((new Exception).getStackTraceString)
				o
		}
	}

	def underlyingValue[T] ( m : Moddable[T] ) : Option[T] = {
		m match {
			case wm : WrappingModdable[T] => underlyingValue(wm.modded)
			case mv : ModdedValue[T] => Some(mv.value)
			case _ => None
		}
	}

	def unwrap[T] ( m : Moddable[T] ) : Moddable[T] = {
		m match {
			case wm : WrappingModdable[T] => wm.modded
			case other => other
		}
	}

	def forward[T] ( m : () => Moddable[T]) = new ForwardingModdable[T](m)
}

object ImplicitModdable {
	implicit def rawToModdable[T](t : T) : Moddable[T] = Moddable(t)
	implicit def funcToModdable[T](f : () => T) : Moddable[T] = Moddable(f)
}
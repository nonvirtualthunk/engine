package arx.core.datastructures

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.vec.Vec2T
import arx.core.vec.Vec3T

import scalaxy.loops._

class Watcher[T] ( value : => T ) {
	var last : T = value
	def hasChanged = {
		val cur = value
		if ( last != cur ) {
			last = cur
			true
		} else {
			false
		}
	}
	def peekChanged = {
		last != value
	}
}

object Watcher {
	def apply[T] (value : => T) = new Watcher(value)

	def apply[T] (x : => T, y : => T, z : => T) = new Watcher3(x,y,z)
	def apply[T] (v : => Vec3T[T]) = new Watcher3(v.x,v.y,v.z)

	def apply[T] (x : => T, y : => T) = new Watcher2(x,y)
	def apply[T] (v : => Vec2T[T]) = new Watcher2(v.x,v.y)
}

class Watcher3[T] (x : => T, y : => T, z : => T) {
	val last = Vec3T[T](x,y,z)
	val cur = Vec3T[T](x,y,z)
	def hasChanged(axis : Int) = {
		cur(axis) = axis match {
			case 0 => x
			case 1 => y
			case 2 => z
		}
		if (last(axis) != cur(axis)) {
			last(axis) = cur(axis)
			true
		} else {
			false
		}
	}
	def peekChanged : Boolean = peekChanged(0) || peekChanged(1) || peekChanged(2)
	def hasChanged : Boolean = hasChanged(0) || hasChanged(1) || hasChanged(2)
	def peekChanged(axis : Int) : Boolean = axis match {
		case 0 => x != last.x
		case 1 => y != last.y
		case 2 => z != last.z
	}
}

class Watcher2[T] (x : => T, y : => T) {
	val last = Vec2T[T](x,y)
	val cur = Vec2T[T](x,y)
	def hasChanged(axis : Int) = {
		cur(axis) = axis match {
			case 0 => x
			case 1 => y
		}
		if (last(axis) != cur(axis)) {
			last(axis) = cur(axis)
			true
		} else {
			false
		}
	}
	def hasChanged : Boolean = hasChanged(0) || hasChanged(1)
	def peekChanged : Boolean = peekChanged(0) || peekChanged(1)
	def peekChanged(axis : Int) : Boolean = axis match {
		case 0 => x != last.x
		case 1 => y != last.y
	}
}
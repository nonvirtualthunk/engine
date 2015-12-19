package arx.core.vec

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 12/16/12
 * Time: 8:43 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto

object functions {
	def length ( v : Vec3f ) = v.length
	def length ( v : Vec3i ) = v.length
	def length ( v : Vec2f ) = v.length
	def length ( v : Vec2i ) = v.length

	def min ( a : Vec3f , b : Vec3f ) = a.min(b)
	def max ( a : Vec3f , b : Vec3f ) = a.max(b)

	def min ( a : Vec3i , b : Vec3i ) = a.min(b)
	def max ( a : Vec3i , b : Vec3i ) = a.max(b)

	def normalize ( a : Vec3f ) = a.normalize
	def normalize ( a : Vec2f ) = a.normalize

	def dot ( a : Vec3f , b : Vec3f ) = a.dot(b)
	def dot ( a : Vec2f , b : Vec2f ) = a.dot(b)

	def cross ( a : Vec3f , b : Vec3f ) = a.cross(b)
}
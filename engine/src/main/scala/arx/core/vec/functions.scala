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
	def length ( v : ReadVec3f ) = v.length
	def length ( v : ReadVec3i ) = v.length
	def length ( v : ReadVec2f ) = v.length
	def length ( v : ReadVec2i ) = v.length

	def min ( a : ReadVec3f , b : ReadVec3f ) = a.min(b)
	def max ( a : ReadVec3f , b : ReadVec3f ) = a.max(b)

	def min ( a : ReadVec3i , b : ReadVec3i ) = a.min(b)
	def max ( a : ReadVec3i , b : ReadVec3i ) = a.max(b)

	def normalize ( a : ReadVec3f ) = a.normalize
	def normalize ( a : ReadVec2f ) = a.normalize

	def dot ( a : ReadVec3f , b : ReadVec3f ) = a.dot(b)
	def dot ( a : ReadVec2f , b : ReadVec2f ) = a.dot(b)

	def cross ( a : ReadVec3f , b : ReadVec3f ) = a.cross(b)
	def cross ( a : ReadVec2f , b : ReadVec2f ) = a.cross(b)
}
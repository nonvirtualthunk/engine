package arx.graphics.pov

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 12/17/15
  * Time: 2:32 PM
  */

import arx.application.Noto
import arx.core.geometry.Plane
import arx.core.mat.ReadMat4x4
import arx.core.math.Recti
import arx.core.traits.TSentinel
import arx.core.traits.TSentinelable
import arx.core.units.UnitOfTime
import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec3f
import arx.core.vec.Vec3f
import arx.engine.EngineCore
import arx.engine.control.event.Event.{Keymap, TEventUser}
import arx.graphics.GL
import arx.graphics.shader.Shader

trait TCamera extends TEventUser with TSentinelable {
	def modelviewMatrix: ReadMat4x4 = modelviewMatrix(GL.viewport)
	def modelviewMatrix(viewport : Recti): ReadMat4x4
	def projectionMatrix : ReadMat4x4 = projectionMatrix(GL.viewport)
	def projectionMatrix(viewport : Recti) : ReadMat4x4

	def eye: ReadVec3f

	def forward: ReadVec3f

	def ortho: ReadVec3f

	def up: ReadVec3f

	var near = 0.1f
	var viewDistance = 100.0f

	def far = viewDistance * 1.1f

	var fovy = 50.0f

	def update(dt: UnitOfTime)

	def moveEyeTo(eye : ReadVec3f)
	def setMoveSpeed(multiplier : ReadVec3f)

	def look() = {
		Shader.boundShader match {
			case Some(shader) => {
				shader.setUniform("ModelViewMatrix", modelviewMatrix, tolerateAbsence = true)
				shader.setUniform("ProjectionMatrix", projectionMatrix, tolerateAbsence = true)
			}
			case None => Noto.warn("No shader bound when camera look is applied")
		}
	}

	def unproject(windowCoord: ReadVec3f, viewport: Recti = GL.viewport) = {
		val modifiedCoord = Vec3f(windowCoord)
		modifiedCoord.y = EngineCore.pixelHeight - windowCoord.y * EngineCore.pixelScaleFactor - 1
		modifiedCoord.x *= EngineCore.pixelScaleFactor
//		Noto.info(s"Unprojecting, raw $windowCoord, effective $modifiedCoord, viewport $viewport")
		GL.unproject(modifiedCoord, modelviewMatrix(viewport), projectionMatrix(viewport), viewport)
	}

	def unprojectAtZ(windowCoord: ReadVec2f, atZ : Float, viewport: Recti = GL.viewport) = {
		val gameNear = unproject(Vec3f(windowCoord,0.0f),viewport)
		val gameFar = unproject(Vec3f(windowCoord,1.0f),viewport)

		val inventoryPlane = new Plane(Vec3f(0.0f,0.0f,atZ), Vec3f.UnitZ)
		val intersection = inventoryPlane.intersect(gameNear, gameFar)
		if (intersection.numIntersections > 0) {
			Some(gameNear + (gameFar - gameNear) * intersection.head)
		} else {
			None
		}
	}

	def keymapNamespace : String
	def isMappingActive(str : String) = Keymap.mappingActive(keymapNamespace, str)
	def deltaFromMappings(neg : String, pos : String, magnitude : Float) = {
		val posActive = isMappingActive(pos)
		val negActive = isMappingActive(neg)
		if (!posActive && !negActive) {
			0.0f
		} else if (posActive) {
			magnitude
		} else {
			-magnitude
		}
	}
}

object TCamera {
	val Sentinel = new EyeCamera() with TSentinel
}

package arx.graphics.pov

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/17/15
 * Time: 2:34 PM
 */

import arx.Prelude._
import arx.core.mat.Mat3x4
import arx.core.metrics.Metrics
import arx.core.units.UnitOfTime
import arx.core.vec.ReadVec3f
import arx.core.vec.Vec2f
import arx.core.vec.Vec3f
import arx.engine.control.event.KeyCombination
import arx.engine.control.event.KeyPressEvent
import arx.engine.control.event.Keymap
import arx.graphics.GL
import org.lwjgl.glfw.GLFW
import scalaxy.loops._

class EyeCamera(var eye : ReadVec3f = Vec3f(0,0,-1), var baseForward : ReadVec3f = Vec3f(0,0,1), var baseUp : ReadVec3f = Vec3f(0,1,0)) extends TCamera {
	var angles: Vec2f = Vec2f(0.0f,0.0f)
	var forward : ReadVec3f = baseForward
	var ortho = baseUp.cross(baseForward)
	var up = baseUp

	var useGlobalUp = false
	var moveSpeed = Vec3f(1.0f,1.0f,1.0f)
	var turnSpeed = Vec2f(1.0f,1.0f)

	var fovy = 50.0f

	def keyCombinationStr = "camera"

	def modelviewMatrix = GL.lookAt(eye,eye + forward,up)
	def projectionMatrix = GL.perspective(fovy,GL.viewport.w/GL.viewport.h.toFloat,near,far)

	var deltaAngles : Vec2f = Vec2f(0.0f,0.0f)
	var deltaEye : Vec3f = Vec3f(0.0f,0.0f,0.0f)

	import EyeCamera._
	onEvent {
		case kpe: KeyPressEvent => {
			Keymap.mappingFor(kpe) match {
				case Some(PanRight) => deltaAngles.x = -1.0f
				case Some(PanLeft) => deltaAngles.x = 1.0f
				case Some(PanUp) => deltaAngles.y = -1.0f
				case Some(PanDown) => deltaAngles.y = 1.0f
				case Some(MoveForward) => deltaEye.x = 1.0f
				case Some(MoveBack) => deltaEye.x = -1.0f
				case Some(MoveRight) => deltaEye.y = 1.0f
				case Some(MoveLeft) => deltaEye.y = -1.0f
				case Some(MoveUp) => deltaEye.z = 1.0f
				case Some(MoveDown) => deltaEye.z = -1.0f
				case _ => // do nothing
			}
		}
	}

	def update (dt: UnitOfTime) {
		manualUpdate()
	}

	// We apparently don't trust the ∂t we're given so we compute it ourselves absolutely
	var lastManualUpdate = -1.0
	def manualUpdate() {
		if ( lastManualUpdate < 0 ) { lastManualUpdate = System.nanoTime() }
		else {
			Metrics.timer("Camera.initialChecks").timeStmt {
				if ( ! Keymap.mappingActive(PanLeft) && ! Keymap.mappingActive(PanRight) ) { deltaAngles.x = 0.0f }
				if ( ! Keymap.mappingActive(PanDown) && ! Keymap.mappingActive(PanUp) ) { deltaAngles.y = 0.0f }

				if ( ! Keymap.mappingActive(MoveForward) && ! Keymap.mappingActive(MoveBack) ) { deltaEye.x = 0.0f }
				if ( ! Keymap.mappingActive(MoveLeft) && ! Keymap.mappingActive(MoveRight) ) { deltaEye.y = 0.0f }
				if ( ! Keymap.mappingActive(MoveUp) && ! Keymap.mappingActive(MoveDown) ) { deltaEye.z = 0.0f }
			}

			Metrics.timer("Camera.computation").timeStmt {
				val curTime = System.nanoTime()
				val f = ((curTime - lastManualUpdate) / 1.66667e7).toFloat
				lastManualUpdate = curTime

				if ( useGlobalUp ) {
					val forwardLength = (forward * Vec3f(1.0f,1.0f,0.0f)).lengthSafe
					val tforward = forward * Vec3f(1.0f,1.0f,0.0f) * deltaEye.x
					val tortho = ortho * Vec3f(1.0f,1.0f,0.0f) * deltaEye.y * forwardLength //adjust the orthos speed to be the same as the forward movement would be
					//this is needed because ortho will always be pure x/y when using a global
					//up, since it is derived from (up x forward), whereas forward is derived
					//from the transform, so looking down results in slower movement
					val tup = Vec3f(0.0f,0.0f,1.0f) * deltaEye.z
					eye += (tforward * moveSpeed.x + tortho * moveSpeed.y + tup * moveSpeed.z) * f
				} else {
					eye += ((forward * deltaEye.x * moveSpeed.x) + (up * deltaEye.z * moveSpeed.z) + (ortho * deltaEye.y * moveSpeed.y)) * f
				}

				angles += deltaAngles * f * 0.025f * turnSpeed


				val transform = (Mat3x4 rotateY angles.y) rotateZ angles.x
				forward = transform transformVector baseForward
				up = transform transformVector baseUp
				ortho = up cross forward
			}
		}
	}
}

object EyeCamera {
	val PanLeft = "EyeCamera.panLeft"
	val PanRight = "EyeCamera.panRight"
	val PanUp = "EyeCamera.panUp"
	val PanDown = "EyeCamera.panDown"
	
	val MoveLeft = "EyeCamera.moveLeft"
	val MoveRight = "EyeCamera.moveRight"
	val MoveUp = "EyeCamera.moveUp"
	val MoveDown = "EyeCamera.moveDown"
	val MoveForward = "EyeCamera.moveForward"
	val MoveBack = "EyeCamera.moveBack"
	
	Keymap.register(PanLeft, GLFW.GLFW_KEY_LEFT)
	Keymap.register(PanRight, GLFW.GLFW_KEY_RIGHT)
	Keymap.register(PanUp, GLFW.GLFW_KEY_UP)
	Keymap.register(PanDown, GLFW.GLFW_KEY_DOWN)

	Keymap.register(MoveLeft, GLFW.GLFW_KEY_A)
	Keymap.register(MoveRight, GLFW.GLFW_KEY_D)
	Keymap.register(MoveForward, GLFW.GLFW_KEY_W)
	Keymap.register(MoveBack, GLFW.GLFW_KEY_S)
	Keymap.register(MoveUp, GLFW.GLFW_KEY_E)
	Keymap.register(MoveDown, GLFW.GLFW_KEY_Q)
}
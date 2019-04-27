package arx.graphics.pov

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/17/15
 * Time: 2:34 PM
 */

import arx.Prelude._
import arx.core.mat.Mat3x4
import arx.core.math.Recti
import arx.core.metrics.Metrics
import arx.core.units.UnitOfTime
import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec3f
import arx.core.vec.Vec2f
import arx.core.vec.Vec3f
import arx.engine.control.event.Event.KeyPressEvent
import arx.engine.control.event.Event.Keymap
import arx.graphics.GL
import org.lwjgl.glfw.GLFW

class EyeCamera(var eye : ReadVec3f = Vec3f(0,0,-1), var baseForward : ReadVec3f = Vec3f(0,0,1), var baseUp : ReadVec3f = Vec3f(0,1,0)) extends TCamera {
	var angles: Vec2f = Vec2f(0.0f,0.0f)
	var forward : ReadVec3f = baseForward
	var ortho = baseForward.cross(baseUp)
	var up = baseUp

	var useGlobalUp = false
	var moveSpeed = Vec3f(1.0f,1.0f,1.0f)
	var turnSpeed = Vec2f(1.0f,1.0f)

	def effectiveMoveSpeed : ReadVec3f = moveSpeed
	def effectiveTurnSpeed : ReadVec2f = turnSpeed

	def keyCombinationStr = "camera"

	def modelviewMatrix(viewport : Recti) = GL.lookAt(eye,eye + forward,up)
	def projectionMatrix(viewport : Recti) = GL.perspective(fovy,viewport.w/viewport.h.toFloat,near,far)


	override def moveEyeTo(eye: ReadVec3f): Unit = { this.eye = eye }

	override def setMoveSpeed(multiplier: ReadVec3f): Unit = {
		moveSpeed = multiplier
	}

	var deltaAngles : Vec2f = Vec2f(0.0f,0.0f)
	var deltaEye : Vec3f = Vec3f(0.0f,0.0f,0.0f)

	var eyeAccel = 0.0f
	var turnAccel = 0.0f

	import EyeCamera._

	onEvent {
		case kpe: KeyPressEvent => handleKey(kpe)
	}

	override def keymapNamespace = EyeCamera.namespace

	def handleKey(kpe: KeyPressEvent): Unit = {
		Keymap.mappingFor(kpe, keymapNamespace) match {
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

	override def look(): Unit = {
		manualUpdate();
		super.look();
	}

	def update (dt: UnitOfTime) {
//		manualUpdate()
	}

	// We apparently don't trust the ∂t we're given so we compute it ourselves absolutely
	var lastManualUpdate = -1.0
	def manualUpdate() {
		if ( lastManualUpdate < 0 ) { lastManualUpdate = System.nanoTime() }
		else {
			deltaAngles.x = deltaFromMappings(PanRight,PanLeft, 1.0f)
			deltaAngles.y = deltaFromMappings(PanUp,PanDown, 1.0f)
			deltaEye.x = deltaFromMappings(MoveBack, MoveForward, 1.0f)
			deltaEye.y = deltaFromMappings(MoveLeft, MoveRight, 1.0f)
			deltaEye.z = deltaFromMappings(MoveDown, MoveUp, 1.0f)

			val curTime = GLFW.glfwGetTime()
			val f = ((curTime - lastManualUpdate) / 0.0166666667).toFloat
			lastManualUpdate = curTime

			Metrics.histogram("Camera.delta").update((f * 1000).toInt)

			val effMS = effectiveMoveSpeed
			val effTS = effectiveTurnSpeed

			if (deltaEye.x.abs < 0.01f && deltaEye.y.abs < 0.01f && deltaEye.z.abs < 0.01f) {
				eyeAccel = 0.1f
			} else {
				if (eyeAccel < 1.0f) {
					eyeAccel += 0.15f
				}
			}

			if (deltaAngles.x.abs < 0.01f && deltaAngles.y.abs < 0.01f) {
				turnAccel = 0.1f
			} else {
				if (turnAccel < 1.0f) {
					turnAccel = (turnAccel * 2.0f).clamp(0.0f,1.0f)
				}
			}

			if ( useGlobalUp ) {
				val forwardLength = (forward * Vec3f(1.0f,1.0f,0.0f)).lengthSafe
				val tforward = forward * Vec3f(1.0f,1.0f,0.0f) * deltaEye.x
				val tortho = ortho * Vec3f(1.0f,1.0f,0.0f) * deltaEye.y * forwardLength //adjust the orthos speed to be the same as the forward movement would be
				//this is needed because ortho will always be pure x/y when using a global
				//up, since it is derived from (up x forward), whereas forward is derived
				//from the transform, so looking down results in slower movement
				val tup = Vec3f(0.0f,0.0f,1.0f) * deltaEye.z
				eye += (tforward * effMS.x + tortho * effMS.y + tup * effMS.z) * f * eyeAccel
			} else {
				eye += ((forward * deltaEye.x * effMS.x) + (up * deltaEye.z * effMS.z) + (ortho * deltaEye.y * effMS.y)) * f * eyeAccel
			}

			angles += deltaAngles * f * 0.025f * effTS * turnAccel


			val transform = (Mat3x4 rotateY angles.y) rotateZ angles.x
			forward = transform transformVector baseForward
			up = transform transformVector baseUp
			ortho = forward cross up
		}
	}
}

object EyeCamera {
	val PanLeft = "panLeft"
	val PanRight = "panRight"
	val PanUp = "panUp"
	val PanDown = "panDown"
	
	val MoveLeft = "moveLeft"
	val MoveRight = "moveRight"
	val MoveUp = "moveUp"
	val MoveDown = "moveDown"
	val MoveForward = "moveForward"
	val MoveBack = "moveBack"

	val namespace = "EyeCamera"
	
	Keymap.register(namespace, PanLeft, GLFW.GLFW_KEY_LEFT)
	Keymap.register(namespace, PanRight, GLFW.GLFW_KEY_RIGHT)
	Keymap.register(namespace, PanUp, GLFW.GLFW_KEY_UP)
	Keymap.register(namespace, PanDown, GLFW.GLFW_KEY_DOWN)

	Keymap.register(namespace, MoveLeft, GLFW.GLFW_KEY_A)
	Keymap.register(namespace, MoveRight, GLFW.GLFW_KEY_D)
	Keymap.register(namespace, MoveForward, GLFW.GLFW_KEY_W)
	Keymap.register(namespace, MoveBack, GLFW.GLFW_KEY_S)
	Keymap.register(namespace, MoveUp, GLFW.GLFW_KEY_E)
	Keymap.register(namespace, MoveDown, GLFW.GLFW_KEY_Q)
}
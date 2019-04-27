package arx.engine.control.data

import arx.application.Noto
import arx.engine.control.components.{ControlModeComponent, TControlMode, TLControlMode}

import scala.collection.immutable.Stack

/**
  * TODO: Add javadoc
  */

class ControlModes extends TControlData{
	var modeStack = Stack[TControlMode]()

	def popMode(mode: TControlMode): Unit = {
		if (modeStack.headOption.contains(mode)) {
			modeStack = modeStack.pop
			mode.deactivate()
		} else {
			Noto.warn("Attempted to pop mode " + mode + " from the mode stack, but it was not topmost")
		}
	}

	def pushMode(mode : TControlMode): Unit = {
		modeStack = modeStack push mode
		mode.activate()
	}
}


class LControlModes extends TControlData{
	var modeStack = Stack[TLControlMode]()

	def popMode(mode: TLControlMode): Unit = {
		if (modeStack.headOption.contains(mode)) {
			modeStack = modeStack.pop
			mode.deactivate()
		} else {
			Noto.warn("Attempted to pop mode " + mode + " from the mode stack, but it was not topmost")
		}
	}

	def pushMode(mode : TLControlMode): Unit = {
		modeStack = modeStack push mode
		mode.activate()
	}
}

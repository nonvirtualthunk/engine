package arx.core.async

object OnExitRegistry {
	var onExitFunctions = Vector[() => Unit]()

	def register(function : () => Unit): Unit = {
		onExitFunctions :+= function
	}

	def onExit(): Unit = {
		onExitFunctions.foreach(f => f())
	}
}

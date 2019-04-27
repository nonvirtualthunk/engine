package arx.application

import java.io.{File, FileOutputStream, FileWriter}

import annotation.elidable
import java.lang.String
import java.util.concurrent.Executors

import arx.core.async.OnExitRegistry

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 11/9/11
 * Time: 5:22 PM
 * Created by nonvirtualthunk
 */

object Noto {
	val None = -2
	val Info = 0
	val Fine = 2
	val Finest = 4

	var indentation = 0

	var debugOn = "true".equals(System.getProperty("logDebug"))
	def finestOn = globalLoggingLevel >= Finest
	def fineOn = globalLoggingLevel >= Fine
	def infoOn = globalLoggingLevel >= Info

	var globalLoggingLevel =
		if ( "true".equals(System.getProperty("logFinest")) ) { Finest }
		else if ( "true".equals(System.getProperty("logFine")) ) { Fine }
		else { Info }

	val logFile = new File("log.txt")
	val logWriter = new FileWriter(logFile)
	val logToFile = false

	var listeners : List[ (String,Int) => Unit ] = Nil

	val executor = Executors.newSingleThreadExecutor()

	def printIndentation(): Unit = {
		var i = 0
		while (i < indentation) {
			print("\t")
			i += 1
		}
	}

	def printMsg(msg : String): Unit = {
		executor.execute(new Runnable {
			override def run(): Unit = {
				printIndentation()
				println(msg)
			}
		})
	}

	@elidable(elidable.INFO) def info ( msg : => String ) {
		if ( infoOn ) {
			printMsg(msg)
			for ( listener <- listeners ) { listener(msg + "\n",Info) }
			if ( debugOn || finestOn || fineOn ) {
				writeToFile(msg + "\n")
			}
		}
	}

	@elidable(elidable.INFO) def info ( llp : TLoggingLevelProvider , msg : => String ) {
		if ( llp.loggingLevel >= Noto.Info ) {
			printMsg(msg)
			for ( listener <- listeners ) { listener(msg + "\n",Info) }
			if ( debugOn || finestOn || fineOn || llp.loggingLevel > Noto.Info ) {
				writeToFile(msg + "\n")
			}
		}
	}

	@elidable(elidable.FINE) def debug ( msg : => String ) {
		if ( debugOn ) {
			printMsg(msg)
			for ( listener <- listeners ) { listener(msg + "\n",Info) }
			writeToFile(msg + "\n")
		}
	}

	@elidable(elidable.FINEST) def finest ( llp: TLoggingLevelProvider , msg : => String ) {
		if ( llp.loggingLevel >= Noto.Finest ) {
			printMsg(msg)
			for ( listener <- listeners ) { listener(msg + "\n",Finest) }
			writeToFile(msg + "\n")
		}
	}
	@elidable(elidable.FINEST) def finest ( msg : => String ) {
		if ( finestOn ) {
			printMsg(msg)
			for ( listener <- listeners ) { listener(msg + "\n",Finest) }
			writeToFile(msg + "\n")
		}
	}

	@elidable(elidable.FINE) def fine ( llp: TLoggingLevelProvider, msg : => String ) {
		if ( llp.loggingLevel >= Noto.Fine ) {
			printMsg(msg)
			for ( listener <- listeners ) { listener(msg + "\n",Fine) }
			writeToFile(msg + "\n")
		}
	}
	@elidable(elidable.FINE) def fine ( msg : => String , appendNewLine : Boolean = true ) {
		if ( finestOn || fineOn ) {
			if ( appendNewLine ) {
				printMsg(msg)
				for ( listener <- listeners ) { listener(msg + "\n",Fine) }
				writeToFile(msg + "\n")
			} else {
				print(msg)
				writeToFile(msg)
			}
		}
	}

	@elidable(elidable.WARNING) def warn ( msg : => String ) {
		printMsg("<warning> " + msg)
		for ( listener <- listeners ) { listener("<warning> " + msg + "\n",None) }
		writeToFile("<warning> " + msg + "\n")
	}
	def error ( msg : => String ) {
		printMsg("<error> " + msg)
		writeToFile("<error> " + msg + "\n")
	}

	def severeError ( msg : => String ) {
		val e = new Exception()
		val effectiveMessage = msg + "\nstack trace:\n" + e.getStackTraceString
		printMsg("<error> " + effectiveMessage)
		for ( listener <- listeners ) { listener("<error> " + effectiveMessage+ "\n",None) }
		writeToFile("<error> " + effectiveMessage)
	}


	def writeToFile ( msg : => String ) {
		if ( logToFile ) {
			executor.execute(new Runnable() {
				override def run(): Unit = logWriter.write(msg)
			})
		}
	}

	OnExitRegistry.register(() => executor.shutdown())
}

trait TLoggingLevelProvider {
	def loggingLevel : Int = Noto.Info
}
class SimpleLoggingLevelProvider(val parent : Option[TLoggingLevelProvider]) extends TLoggingLevelProvider {
	def this() { this(None) }

	var _loggingLevel = Noto.Info
	override def loggingLevel = parent match {
		case Some(p) => math.max(p.loggingLevel, this._loggingLevel)
		case None => this._loggingLevel
	}
	def loggingLevel_= ( l : Int ) { _loggingLevel = l }
}
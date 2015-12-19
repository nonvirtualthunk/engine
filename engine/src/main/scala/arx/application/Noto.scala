package arx.application

import java.io.{FileWriter, FileOutputStream, File}
import annotation.elidable
import java.lang.String

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


	@elidable(elidable.INFO) def info ( msg : => String ) {
		if ( infoOn ) {
			println(msg)
			for ( listener <- listeners ) { listener(msg + "\n",Info) }
			if ( debugOn || finestOn || fineOn ) {
				writeToFile(msg + "\n")
			}
		}
	}

	@elidable(elidable.INFO) def info ( llp : TLoggingLevelProvider , msg : => String ) {
		if ( llp.loggingLevel >= Noto.Info ) {
			println(msg)
			for ( listener <- listeners ) { listener(msg + "\n",Info) }
			if ( debugOn || finestOn || fineOn || llp.loggingLevel > Noto.Info ) {
				writeToFile(msg + "\n")
			}
		}
	}

	@elidable(elidable.FINE) def debug ( msg : => String ) {
		if ( debugOn ) {
			println(msg)
			for ( listener <- listeners ) { listener(msg + "\n",Info) }
			writeToFile(msg + "\n")
		}
	}

	@elidable(elidable.FINEST) def finest ( llp: TLoggingLevelProvider , msg : => String ) {
		if ( llp.loggingLevel >= Noto.Finest ) {
			println(msg)
			for ( listener <- listeners ) { listener(msg + "\n",Finest) }
			writeToFile(msg + "\n")
		}
	}
	@elidable(elidable.FINEST) def finest ( msg : => String ) {
		if ( finestOn ) {
			println(msg)
			for ( listener <- listeners ) { listener(msg + "\n",Finest) }
			writeToFile(msg + "\n")
		}
	}

	@elidable(elidable.FINE) def fine ( llp: TLoggingLevelProvider, msg : => String ) {
		if ( llp.loggingLevel >= Noto.Fine ) {
			println(msg)
			for ( listener <- listeners ) { listener(msg + "\n",Fine) }
			writeToFile(msg + "\n")
		}
	}
	@elidable(elidable.FINE) def fine ( msg : => String , appendNewLine : Boolean = true ) {
		if ( finestOn || fineOn ) {
			if ( appendNewLine ) {
				println(msg)
				for ( listener <- listeners ) { listener(msg + "\n",Fine) }
				writeToFile(msg + "\n")
			} else {
				print(msg)
				writeToFile(msg)
			}
		}
	}

	@elidable(elidable.WARNING) def warn ( msg : => String ) {
		println("<warning> " + msg)
		for ( listener <- listeners ) { listener("<warning> " + msg + "\n",None) }
		writeToFile("<warning> " + msg + "\n")
	}
	def error ( msg : => String ) {
		println("<error> " + msg)
		writeToFile("<error> " + msg + "\n")
	}

	def severeError ( msg : => String ) {
		val e = new Exception()
		val effectiveMessage = msg + "\nstack trace:\n" + e.getStackTraceString
		println("<error> " + effectiveMessage)
		for ( listener <- listeners ) { listener("<error> " + effectiveMessage+ "\n",None) }
		writeToFile("<error> " + effectiveMessage)
	}


	def writeToFile ( msg : => String ) {
		if ( logToFile ) {
			logWriter.write(msg)
		}
	}
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
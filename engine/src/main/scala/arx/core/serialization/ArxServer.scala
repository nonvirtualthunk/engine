package arx.core.serialization

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/26/15
 * Time: 6:45 PM
 */

import arx.engine.control.event.Event.Event
import arx.engine.control.event.Event.TEventUser
import com.carrotsearch.hppc._
import com.esotericsoftware.kryonet._
import com.twitter.chill.KryoBase
import arx.Prelude._

class ArxServer(wbs : Int, obs : Int) extends Server(wbs,obs, new ArxKryonetSerialization) with TEventUser {
	getKryo.applyArxDefaultSettings()

	override def newConnection(): Connection = new ArxConnection {}


	addListener(new Listener {
		import NetworkEvents._
		override def connected(connection: Connection): Unit = {
			handleEvent(Connected(connection.asInstanceOf[ArxConnection]))
		}
		override def disconnected(connection: Connection): Unit = {
			handleEvent(Disconnected(connection.asInstanceOf[ArxConnection]))
		}
		override def idle(connection: Connection): Unit = {
			handleEvent(Idle(connection.asInstanceOf[ArxConnection]))
		}
		override def received(connection: Connection, obj: scala.Any): Unit = {
			handleEvent(Received(connection.asInstanceOf[ArxConnection],obj))
		}
	})
}

class ArxClient(wbs : Int, obs : Int) extends Client(wbs,obs,new ArxKryonetSerialization) with ArxConnection with TEventUser {
	getKryo.applyArxDefaultSettings()

	addListener(new Listener {
		import NetworkEvents._
		override def connected(connection: Connection): Unit = {
			handleEvent(Connected(connection.asInstanceOf[ArxConnection]))
		}
		override def disconnected(connection: Connection): Unit = {
			handleEvent(Disconnected(connection.asInstanceOf[ArxConnection]))
		}
		override def idle(connection: Connection): Unit = {
			handleEvent(Idle(connection.asInstanceOf[ArxConnection]))
		}
		override def received(connection: Connection, obj: scala.Any): Unit = {
			handleEvent(Received(connection.asInstanceOf[ArxConnection],obj))
		}
	})
}

trait ArxConnection extends Connection {
	val sentEntities = new LongOpenHashSet()
//	val receivedEntities = new LongObjectOpenHashMap[GameEntity]()
	val sentRootConfigs = new IntOpenHashSet()
}

object NetworkEvents {
	case class Connected(connection : ArxConnection) extends Event
	case class Received(connection : ArxConnection, obj : Any) extends Event
	case class Idle(connection : ArxConnection) extends Event
	case class Disconnected(connection : ArxConnection) extends Event
}

class ArxKryonetSerialization extends KryoSerialization(new KryoBase) {
	
}
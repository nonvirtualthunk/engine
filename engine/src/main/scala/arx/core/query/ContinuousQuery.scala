package arx.core.query

/**
 *
 */

class ContinuousQuery[T <: AnyRef : Manifest] ( val matchFunction : (AnyRef) => Option[T] ) extends Serializable with scala.collection.Traversable[T] {
	def this ( mf : PartialFunction[AnyRef,T] ) { this(mf.lift) }
	@transient var results : Set[T] = Set[T]()
//	@transient val added = new SynchronizedQueue[T]
//	@transient val removed = new SynchronizedQueue[T]
	@transient var source : Option[TContinuousQuerySource] = None

	def add ( x : AnyRef ) {
		matchFunction(x) match {
			case Some(t) => {
				if ( ! results.contains(t) ) {
					results = results + t
	//				added.enqueue(t)
					listeners.foreach( _.queryResultAdded(t) )
				}
			}
			case None =>
		}
	}
	def remove ( x : AnyRef ) {
		matchFunction(x) match {
			case Some(t) => {
				if ( results.contains(t) ) {
					results = results - t
	//				removed.enqueue(t)
					listeners.foreach( _.queryResultRemoved(t) )
				}
			}
			case None =>
		}
	}

	@transient protected var listeners : List[ContinuousQueryListener[T]] = Nil

	/**
	 * Adds a listener to this query and returns itself. If <code>fireOnExistingResults</code> is <code>true</code>
	 * then it will call the listener's <code>queryResultAdded</code> function for all results already
	 * in this query, otherwise only newly added/removed items will trigger the listener.
	 * @return
	 */
	def withListener( l : ContinuousQueryListener[T], fireOnExistingResults : Boolean ) = {
		this.listeners ::= l
		if ( fireOnExistingResults ) {
			results.foreach( l.queryResultAdded )
		}

		this
	}

	def onAddition(f : (T) => Unit, fireOnExistingResults : Boolean): Unit = {
		this.withListener(new ContinuousQueryListener[T] {

			override def queryResultAdded(t: T): Unit = { f(t) }
			override def queryResultRemoved(t: T): Unit = {}
		}, fireOnExistingResults)

	}

	def foreach[U](f: (T) => U) {results.foreach(f)}
	override def size = results.size

	def withQueryFilter (filter : (T) => Boolean) = {
		// we can asInstanceOf[T] because the filtered query will only get results from this query, and so must already
		// be of type T
		new FilteredContinuousQuery[T]({
			(a:AnyRef) => if (filter(a.asInstanceOf[T])) { Some(a.asInstanceOf[T]) } else { None }
		},this)
	}
}
trait ContinuousQueryListener[T] {
	def queryResultAdded ( t : T )
	def queryResultRemoved ( t : T )
}

trait TContinuousQuerySource {
	def registerQuery ( query : ContinuousQuery[_] )
	def unregisterQuery ( query : ContinuousQuery[_] )
}
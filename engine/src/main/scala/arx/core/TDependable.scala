package arx.core

/**
  * Created by IntelliJ IDEA.
  * User: nvt
  * Date: 3/12/12
  * Time: 9:40 PM
  * Created by nonvirtualthunk
  */

import arx.application.Noto
import arx.core.introspection.ReflectionAssistant

trait TDependable {
	var _dependencies: List[Class[_]] = List()

	def dependencies: List[Class[_]] = _dependencies;

	def dependencies_=(s: List[Class[_]]) {
		_dependencies = s
	}

	var resolvedDependencies: List[AnyRef] = Nil

	def reify[T <: AnyRef : Manifest]: T = {
		resolvedDependencies.find(d => manifest[T].erasure.isAssignableFrom(d.getClass)) match {
			case Some(t) => t.asInstanceOf[T]
			case None => {
				if (dependencies.exists(c => manifest[T].erasure.isAssignableFrom(c))) {
					//				Noto.warn("Dependency resolution failed, attempted matches for " + manifest[T].erasure.getSimpleName + ":")
					//				resolvedDependencies.foreach( d => Noto.warn("\t" + d.getClass.getSimpleName + ": " + manifest[T].erasure.isAssignableFrom(d.getClass)) )
					//				Noto.warn("Printall: " + resolvedDependencies)
					//				throw new IllegalStateException("Attempting to get dependency before dependencies have been resolved")
					resolvedDependencies.find(d => manifest[T].erasure.isAssignableFrom(d.getClass)) match {
						case Some(t) => t.asInstanceOf[T]
						case None => if (dependencies.exists(c => manifest[T].erasure.isAssignableFrom(c))) {
							Noto.warn("Dependency resolution failed, attempted matches for " + manifest[T].erasure.getSimpleName + ":")
							resolvedDependencies.foreach(d => if (manifest[T].erasure.isAssignableFrom(d.getClass)) {
								return d.asInstanceOf[T]
							})
							throw new IllegalStateException("Attempting to get dependency before dependencies have been resolved")
						} else {
							throw new IllegalStateException("Attempting to get instance of class not specified as a dependency")
						}
					}
				} else {
					throw new IllegalStateException("Attempting to get instance of class not specified as a dependency")
				}
			}
		}
	}

	def subDependables: List[TDependable] = Nil
}

object Dependency {
	def recursiveResolveDependables(l: List[TDependable]): List[TDependable] = {
		l match {
			case Nil => Nil
			case someList =>
				val newDependables = someList.flatMap(_.subDependables)
				l ::: newDependables ::: recursiveResolveDependables(newDependables)
		}
	}

	def resolve(dependables_a: List[TDependable], resolveAgainst: List[AnyRef], instantiationFunc: (List[Class[_]]) => AnyRef = (l) => ReflectionAssistant.firstInstanceOf(l)): List[AnyRef] = {
		var dependables = recursiveResolveDependables(dependables_a)
		var res = resolveAgainst
		while (dependables.nonEmpty) {
			val generator = dependables.head
			for (dep <- generator.dependencies) {
				generator.resolvedDependencies ::= (res.find(g => dep.isAssignableFrom(g.getClass)) match {
					case Some(g) => g
					case None =>
						val instOpt =
							if (dep.isInterface || java.lang.reflect.Modifier.isAbstract(dep.getModifiers)) {
								instantiationFunc(ReflectionAssistant.allSubTypesOf(dep))
							} else {
								try {
									Some(instantiationFunc(List(dep)))
								} catch {
									case e: Exception => None
								}
							}

						instOpt match {
							case Some(inst) =>
								inst match {
									case newGenerator: TDependable =>
										res ::= newGenerator
										dependables :+= newGenerator
										newGenerator
									case o: AnyRef =>
										res ::= o
										o
									case _ => throw new IllegalStateException("something other than AnyRef encountered during dependency resolution");
								}
							case None => throw new IllegalStateException("Could not find acceptable dependency filler of type : " + dep);
						}
				})
			}
			if (!res.contains(generator)) {
				res ::= generator
			}
			dependables = dependables.tail
		}
		res
	}

	def resolve(dependables_a: List[TDependable]): List[AnyRef] = resolve(dependables_a, dependables_a)

	def resolve(dependable: TDependable, resolveAgainst: List[AnyRef]): List[AnyRef] = resolve(List(dependable), resolveAgainst)

	def topologicalSort[T <: TDependable](dependables: List[T]): List[T] = {
		var res = List[T]()
		var set = Set[T]()

		def add(d: T) {
			if (!set.contains(d)) {
				set += d
				d.resolvedDependencies foreach { case subDependency: T => add(subDependency); case _ => }
				res ::= d
			}
		}

		dependables foreach {
			add
		}
		res = res.reverse.filter { e => dependables.contains(e) }
		res
	}


	class Context {
		var allInstances = List[Any]()
	}

	def resolveByInjection(classes: List[Class[_]], contextIn: List[Any]) = {
		val ctx = new Context
		ctx.allInstances = contextIn

		(classes.map(c => inject(c, ctx)), ctx.allInstances)
	}

	def inject(c: Class[_], ctx: Context): Any = {
		ctx.allInstances.find(inst => c.isAssignableFrom(inst.getClass)) match {
			case Some(existing) => existing
			case None =>
				val constructor = c.getConstructors.maxBy(_.getParameterCount)
				val newInst : Any =
					if (ReflectionAssistant.isSingleton(c)) {
						ReflectionAssistant.getSingleton(c).asInstanceOf[Any]
					} else if (constructor.getParameterCount == 0) {
						ReflectionAssistant.instantiate(c).asInstanceOf[Any]
					} else {
						val parameters = constructor.getParameterTypes.map(pt => inject(pt, ctx).asInstanceOf[AnyRef])
						constructor.newInstance(parameters:_*).asInstanceOf[Any]
					}
				ctx.allInstances ::= newInst
				newInst
		}
	}
}


/*
def resolve(dependables_a: List[TDependable], resolveAgainst: List[AnyRef], instantiationFunc: (List[Class[_]]) => AnyRef = (l) => ReflectionAssistant.firstInstanceOf(l)): List[AnyRef] = {
		var dependables = recursiveResolveDependables(dependables_a)
		var res = resolveAgainst

		def recursiveCreate[T](clazz : Class[T]) : T = {
			val constructor = clazz.getConstructors.maxBy(c => c.getParameterCount)
			val parameters = constructor.getParameterTypes
				.map(t => res.find(g => clazz.isAssignableFrom(g.getClass)) match {
					case Some(result) =>
						result
					case None =>
						recursiveCreate(t)
				})
			val newInst = constructor.newInstance(parameters).asInstanceOf[T]
			res ::= newInst
			newInst
		}

		while (dependables.nonEmpty) {
			val generator = dependables.head
			for (dep <- generator.dependencies) {
				generator.resolvedDependencies ::= (res.find(g => dep.isAssignableFrom(g.getClass)) match {
					case Some(g) => g
					case None =>
						val possibleImpls =
							if (dep.isInterface || java.lang.reflect.Modifier.isAbstract(dep.getModifiers)) {
								ReflectionAssistant.allSubTypesOf(dep)
							} else {
								List(dep)
							}

						val instOpt = possibleImpls match {
							case head :: _ => Some(recursiveCreate(head))
							case Nil => None
						}

						instOpt match {
							case Some(inst) =>
								inst match {
									case newGenerator: TDependable =>
										res ::= newGenerator
										dependables :+= newGenerator
										newGenerator
									case o: AnyRef =>
										res ::= o
										o
									case _ => throw new IllegalStateException("something other than AnyRef encountered during dependency resolution");
								}
							case None => throw new IllegalStateException("Could not find acceptable dependency filler of type : " + dep);
						}
				})
			}
			if (!res.contains(generator)) {
				res ::= generator
			}
			dependables = dependables.tail
		}
		res
	}
 */
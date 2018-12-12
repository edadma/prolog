package xyz.hyperreal.prolog

import java.lang.reflect.{Modifier, Method}

import scala.collection.mutable


object Builtin {

  private val predicates = new mutable.HashMap[Functor, VM => Unit]

  def exists( f: Functor ) = predicates contains f

  def predicate( f: Functor ) = predicates(f)

  def load( obj: Any ): Unit =
    for (m <- obj.getClass.getDeclaredMethods if m.getModifiers == Modifier.PUBLIC && !m.isSynthetic) {
      val name = m.getName

      m.getReturnType match {
        case Void.TYPE =>
        case java.lang.Boolean.TYPE => predicates(functor(name, m.getParameterCount)) = new Predicate(obj, m)
        case _ =>
      }
    }

  class Predicate( obj: Any, method: Method ) extends (VM => Unit) {
    def apply( vm: VM ): Unit = {
      if(!method.invoke( obj, (for (_ <- 1 to method.getParameterCount) yield vm.popValue).reverse.
        toArray.asInstanceOf[Array[Object]]: _* ).asInstanceOf[Boolean])
        vm.fail
    }
  }

  List(
    BultinCharacterIO,
    BuiltinTermIO
  ) foreach load

}

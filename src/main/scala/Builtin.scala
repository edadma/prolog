package xyz.hyperreal.prolog

import java.lang.reflect.{Modifier, Method}

import scala.collection.mutable


object Builtin {

  private val predicates = new mutable.HashMap[Functor, VM => Unit]

  def translate( s: String ) =
    s flatMap {
      case '=' => "$eq"
      case c if !c.isLetter => "$u" + f"$c%04x".toUpperCase
      case c => c.toString
    }

  def translate( f: Functor ): Functor = {
    if (f.name.name.head.isLetter)
      f
    else
      functor( translate(f.name.name), f.arity )
  }

  def exists( f: Functor ) = predicates contains translate( f )

  def predicate( f: Functor ) = predicates(translate( f ))

  def load( obj: Any ): Unit =
    for (m <- obj.getClass.getDeclaredMethods if m.getModifiers == Modifier.PUBLIC && !m.isSynthetic) {
      val name = m.getName

      m.getReturnType match {
        case Void.TYPE =>
        case java.lang.Boolean.TYPE => predicates(functor(name, m.getParameterCount - 1)) = new Predicate(obj, m)
        case _ =>
      }
    }

  class Predicate( obj: Any, method: Method ) extends (VM => Unit) {
    def apply( vm: VM ): Unit =
      if(!method.invoke( obj, (vm +: (for (_ <- 1 until method.getParameterCount) yield vm.pop)).
        toArray.asInstanceOf[Array[Object]]: _* ).asInstanceOf[Boolean])
        vm.fail

    override def toString(): String = s"<predicate ${method.getName}/${method.getParameterCount}>"
  }

  List(
    BultinCharacterIO,
    BuiltinTermIO,
    BuiltinTypeTesting,
    BuiltinTermManipulation,
    BuiltinControl
  ) foreach load

}

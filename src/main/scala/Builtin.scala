package xyz.hyperreal.prolog

import java.lang.reflect.{Method, Modifier}

import scala.collection.mutable


object Builtin {

  private val predicates = new mutable.HashMap[Functor, VM => Unit]
  val segmentRegex = "\\$(?:[a-z]{2,}|u[0-9A-F]{4})"r

  def translate( s: String ) =
    if (s startsWith "$") {
      val buf = new StringBuilder

      for (segment <- segmentRegex.findAllIn( s ))
        segment match {
          case "$eq" => buf += '='
          case u => buf += Integer.parseInt( u.substring(2), 16 ).toChar
        }

      buf.toString
    } else
      s

  def exists( f: Functor ) = predicates contains f

  def predicate( f: Functor ) = predicates(f)

  def load( obj: Any ): Unit =
    for (m <- obj.getClass.getDeclaredMethods if m.getModifiers == Modifier.PUBLIC && !m.isSynthetic) {
      val name = m.getName

      m.getReturnType match {
        case Void.TYPE =>
        case java.lang.Boolean.TYPE => predicates(functor(translate(name), m.getParameterCount - 1)) = new Predicate(obj, m)
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
    builtin.CharacterIO,
    builtin.TermIO,
    builtin.TypeTesting,
    builtin.TermManipulation,
    builtin.Control
  ) foreach load

}
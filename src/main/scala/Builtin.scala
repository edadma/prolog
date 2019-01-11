package xyz.hyperreal.prolog

import java.lang.reflect.{Method, Modifier}

import scala.collection.mutable


object Builtin {

  private val predicates = new mutable.HashMap[Indicator, VM => Unit]
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

  def exists( f: Indicator ) = predicates contains f

  def predicate( f: Indicator ) = predicates(f)

  def load( obj: Any ): Unit =
    for (m <- obj.getClass.getDeclaredMethods if m.getModifiers == Modifier.PUBLIC && !m.isSynthetic) {
      val name = m.getName

      m.getReturnType match {
        case Void.TYPE => predicates(functor(translate(name), m.getParameterCount - 1)) = new Predicate( obj, m, false )
        case java.lang.Boolean.TYPE => predicates(functor(translate(name), m.getParameterCount - 1)) = new Predicate( obj, m, true )
        case _ =>
      }
    }

  class Predicate( obj: Any, method: Method, ret: Boolean ) extends (VM => Unit) {
    def apply( vm: VM ): Unit = {
      val args = new Array[Object]( method.getParameterCount )

      args(0) = vm

      for (i <- method.getParameterCount - 1 to 1 by -1)
        args(i) = vm.pop.asInstanceOf[Object]

      if (ret) {
        if (!method.invoke( obj, args: _* ).asInstanceOf[Boolean])
          vm.fail
      } else
        method.invoke( obj, args: _* )
    }

    override def toString(): String = s"<predicate ${method.getName}/${method.getParameterCount}>"
  }

  List(
    builtin.AtomManipulation,
    builtin.CharacterIO,
    builtin.TermIO,
    builtin.TypeTesting,
    builtin.TermManipulation,
    builtin.Control,
    builtin.StreamSelection,
    builtin.StringManipulation,
    builtin.AllSolutions,
    builtin.ClauseRetrieval,
    builtin.BinaryIO
  ) foreach load

}
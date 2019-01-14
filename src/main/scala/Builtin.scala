package xyz.hyperreal.prolog

import java.lang.reflect.{InvocationTargetException, Method, Modifier}

import xyz.hyperreal.pattern_matcher.Reader

import scala.collection.mutable


object Builtin {

  private val predicates = new mutable.HashMap[Indicator, (VM, IndexedSeq[Reader]) => Unit]
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
        case Void.TYPE => predicates(indicator(translate(name), m.getParameterCount - 2)) = new Predicate( obj, m, false )
        case java.lang.Boolean.TYPE => predicates(indicator(translate(name), m.getParameterCount - 2)) = new Predicate( obj, m, true )
        case _ =>
      }
    }

  class Predicate( obj: Any, method: Method, ret: Boolean ) extends ((VM, IndexedSeq[Reader]) => Unit) {
    def apply( vm: VM, pos: IndexedSeq[Reader] ): Unit = {
      val args = new Array[Object]( method.getParameterCount )

      args(0) = vm
      args(1) = pos

      for (i <- method.getParameterCount - 1 to 2 by -1)
        args(i) = vm.pop.asInstanceOf[Object]

      try {
        if (ret) {
          if (!method.invoke( obj, args: _* ).asInstanceOf[Boolean])
            vm.fail
        } else
            method.invoke( obj, args: _* )
      } catch {
        case e: InvocationTargetException => throw e.getCause
      }
    }

    override def toString = s"<predicate ${method.getName}/${method.getParameterCount}>"
  }

  List(
//    builtin.AtomManipulation,
    builtin.CharacterIO,
//    builtin.TermIO,
//    builtin.TypeTesting,
//    builtin.TermManipulation,
//    builtin.Control,
    builtin.StreamSelection
//    builtin.StringManipulation,
//    builtin.AllSolutions,
//    builtin.ClauseRetrieval,
//    builtin.BinaryIO,
//    builtin.ImplementationDefined
  ) foreach load

}
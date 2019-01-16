package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{Operator, Structure, VM, domainError, typeError}

import scala.collection.mutable


object Operators {

  val optable =
    mutable.HashSet[Operator] (
      Operator( 1200, 'xfx, ':- ),
      Operator( 1200, 'xfx, '--> ),
      Operator( 1200, 'fx, ':- ),
      Operator( 1200, 'fx, '?- ),
      Operator( 1050,	'xfy, '-> ),
      Operator( 1000,	'xfy, Symbol(",") ),
      Operator( 900, 'fy, Symbol("\\+") )
    )

  def op( vm: VM, pos: IndexedSeq[Reader], priority: Any, specifier: Any, operator: Any ) =
    (priority, specifier, operator) match {
      case (p: Int, s: Symbol, _) =>
        true
    }

}
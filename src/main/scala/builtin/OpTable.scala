package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{Operator, Structure, VM, domainError, typeError}


object OpTable {

  def op( vm: VM, pos: IndexedSeq[Reader], priority: Any, specifier: Any, operator: Any ) =
    (priority, specifier, operator) match {
      case (p: Int, s: Symbol, _) =>
        true
      case _ => sys.error( "op/3: expected integer, atom, atom" )
    }

}
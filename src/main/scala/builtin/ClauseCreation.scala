package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{Indicator, Procedure, Structure, VM, indicator}


object ClauseCreation {

  def asserta( vm: VM, pos: IndexedSeq[Reader], clause: Any ) =
    clause match {
      case _: vm.Variable => sys.error( "asserta: clause must be given" )
      case _ =>
    }

}
package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.prolog.{Indicator, Procedure, Structure, VM, indicator}


object ClauseCreation {

  def asserta( vm: VM, clause: Any ) =
    clause match {
      case _: vm.Variable => sys.error( "asserta: clause must be given" )
      case _ =>
    }

}
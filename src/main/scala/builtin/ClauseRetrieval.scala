package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.prolog.{indicator, VM}


object ClauseRetrieval {

  def current_predicate( vm: VM, pred: Any ) =
    vm.prog.procedures match {
      case Nil => false
      case List( p ) => vm.unify( indicator(p.ind), pred )
      case h :: t =>
        vm.resatisfyable(
          new (VM => Boolean) {
            var procs = t

            def apply( v: VM ): Boolean = {
              procs match {
                case
              }
              vm.resatisfyable( this )

              val cur = procs.head

              procs = procs.tail
              vm.unify( indicator(p.ind), pred )
            }
          }
        )
    }

}
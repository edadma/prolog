package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.prolog.{Indicator, Procedure, Structure, VM, indicator}


object ClauseRetrieval {

  def clause( vm: VM, head: Any, body: Any ) = {
    val p =
      head match {
        case _: vm.Variable => sys.error( "clause: head must be given" )
        case a: Symbol => vm.prog get Indicator( a, 0 )
        case s: Structure => vm.prog get s.ind
        case _ => sys.error( "clause: head must be an atom or structure" )
      }

    p match {
      case Some( Procedure(_, _, true, clauses) ) =>
        clauses.toList match {
          case List( c ) => vm.unify( vm.data(c.head), head ) && vm.unify( vm.data(c.body), body )
          case h :: t =>
            vm.resatisfyable(
              new (VM => Boolean) {
                var rest = t

                def apply( v: VM ): Boolean = {
                  rest match {
                    case List( c ) => vm.unify( vm.data(c.head), head ) && vm.unify( vm.data(c.body), body )
                    case rh :: rt =>
                      vm.resatisfyable( this )
                      rest = rt
                      vm.unify( vm.data(rh.head), head ) && vm.unify( vm.data(rh.body), body )
                  }
                }
              }
            )
            vm.unify( vm.data(h.head), head ) && vm.unify( vm.data(h.body), body )
        }
      case _ => false
    }
  }

  def current_predicate( vm: VM, pred: Any ) =
    vm.prog.procedures match {
      case Nil => false
      case List( p ) => vm.unify( indicator(p.ind), pred )
      case h :: t =>
        vm.resatisfyable(
          new (VM => Boolean) {
            var rest = t

            def apply( v: VM ): Boolean = {
              rest match {
                case List( p ) => vm.unify( indicator(p.ind), pred )
                case rh :: rt =>
                  vm.resatisfyable( this )
                  rest = rt
                  vm.unify( indicator(rh.ind), pred )
              }
            }
          }
        )
        vm.unify( indicator(h.ind), pred )
    }

}
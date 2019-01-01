package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.prolog.{CONS, Compound, Functor, NIL, Structure, VM, array2list, cons, list2array}


object TermManipulation {

  def `=..`( vm: VM, list: Any, term: Any ) =
    term match {
      case v: vm.Variable =>
        list match {
          case NIL => sys.error( "univ: empty list" )
          case Structure( CONS, Array(head: Symbol, NIL) ) =>
            v bind head
            true
          case Structure( CONS, Array(head: Symbol, tail) ) =>
            list2array( tail ) match {
              case Some( args ) => v bind Structure( Functor(head, args.length), args )
              case None => sys.error( s"univ: illegal list argument: $list" )
            }

            true
          case _ => sys.error( s"univ: illegal list argument: $list" )
        }
      case a: Symbol => vm.unify( list, cons(a, NIL) )
      case Structure( Functor(name, _), args ) => vm.unify( list, cons(name, array2list(args)) )
      case _ => sys.error( s"univ: illegal term argument: $term" )
    }

  def copy_term( vm: VM, term2: Any, term1: Any ) = vm.unify( vm.copy(term1), term2 )

  def functor( vm: VM, arity: Any, name: Any, term: Any ) =
    term match {
      case v: vm.Variable =>
        name match {
          case s: Symbol =>
            arity match {
              case 0 =>
                v bind s
                true
              case argc: Int =>
                v bind Structure( Functor(s, argc), Array.fill(argc)(new vm.Variable) )
                true
              case _ => sys.error( "functor: arity must be integer" )
            }
          case _ => sys.error( "functor: name must be atom" )
        }
      case s: Symbol => vm.unify( name, s ) && vm.unify( arity, 0 )
      case Structure( Functor(sname, sarity), _ ) => vm.unify( name, sname ) && vm.unify( arity, sarity )
    }

  def arg( vm: VM, arg: Any, term: Any, n: Any ) = {
    n match {
      case idx: Integer =>
        term match {
          case compound: Compound => vm.unify( compound productElement idx, arg )
          case _ => sys.error( "arg: second argument must be compound" )
        }
      case _ => sys.error( "arg: first argument must be integer" )
    }
  }

}

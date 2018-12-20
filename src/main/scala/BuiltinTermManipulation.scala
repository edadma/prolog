package xyz.hyperreal.prolog


object BuiltinTermManipulation {

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
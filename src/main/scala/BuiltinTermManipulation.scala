package xyz.hyperreal.prolog

import BuiltinTypeTesting._


object BuiltinTermManipulation {

//  def functor( a: Any ) =

  def arg( vm: VM, arg: AnyRef, term: AnyRef, n: AnyRef ) = {
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
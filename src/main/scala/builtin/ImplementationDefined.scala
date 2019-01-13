package xyz.hyperreal.prolog

import scala.collection.mutable


object ImplementationDefined {

  val flags =
    mutable.HashMap[Symbol, Any] (
      'bounded -> true,
      'integer_rounding_function -> 'toward_zero,
      'debug -> 'off,
      'max_arity -> 255
    )
  val changeable = Set( 'debug )

  def set_prolog_flag( vm: VM, flag: Any, value: Any ) =
    (flag, value) match {
      case (_: vm.Variable, _: vm.Variable) => sys.error( "set_prolog_flag/2: flag and value must be given" )
      case (f: Symbol, _) if flags contains f =>

      case _ => sys.error( "set_prolog_flag/2: expected valid flag and value" )
    }

}
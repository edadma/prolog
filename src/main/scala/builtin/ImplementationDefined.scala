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

  def set_prolog_flag( vm: VM, flag: Any, value: Any )

}
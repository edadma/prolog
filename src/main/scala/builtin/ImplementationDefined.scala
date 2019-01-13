package xyz.hyperreal.prolog

import scala.collection.mutable


object ImplementationDefined {

  class Flag( val default: Any ) {
    val changeable = false

    val possible: Any => Boolean = Set[Any]()

    def value = default

    def value_=( newvalue: Any ): Unit = sys.error( "unchangeable flag" )
  }

  class ChangeableFlagSet( default: Any, override val possible: Set[Any], action: => Unit = {} ) extends Flag( default ) {
    var v: Any = default

    override def value = v

    override def value_=( newvalue: Any ) = v = newvalue
  }

  val flags =
    mutable.HashMap[Symbol, Flag] (
      'bounded -> new Flag( true ),
      'integer_rounding_function -> new Flag( 'toward_zero ),
      'debug -> new ChangeableFlagSet( 'off, Set[Any]('off, 'on) ),
      'max_arity -> new Flag( 255 )
    )
  val changeable = Set( 'debug )

  def set_prolog_flag( vm: VM, flag: Any, value: Any ) =
    (flag, value) match {
      case (_: vm.Variable, _: vm.Variable) => sys.error( "set_prolog_flag/2: flag and value must be given" )
      case (f: Symbol, _) if flags.contains(f) && flags(f).changeable =>
        flags(f) = value
      case _ => sys.error( "set_prolog_flag/2: expected valid changeable flag and value" )
    }

}
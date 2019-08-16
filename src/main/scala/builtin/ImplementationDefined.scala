package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{VM, symbolOrdering}

import scala.collection.immutable.SortedMap



object ImplementationDefined {

  class Flag( val default: Any ) {
    val changeable = false

    val valid: Any => Boolean = Set[Any]()

    def value = default

    def value_=( newvalue: Any ): Unit = sys.error( "unchangeable flag" )
  }

  class ChangeableFlag( default: Any, override val valid: Set[Any], action: Any => Unit = _ => () ) extends Flag( default ) {
    var v: Any = default

    override def value = v

    override def value_=( newvalue: Any ) = {
      v = newvalue
      action( newvalue )
    }
  }

  val flags =
    SortedMap[Symbol, Flag] (
      Symbol("bounded") -> new Flag( true ),
      Symbol("integer_rounding_function") -> new Flag( Symbol("toward_zero") ),
      Symbol("debug") -> new ChangeableFlag( Symbol("off"), Set[Any](Symbol("off"), Symbol("on")) ),
      Symbol("max_arity") -> new Flag( 255 )
    )

  def set_prolog_flag( vm: VM, pos: IndexedSeq[Reader], flag: Any, value: Any ) =
    (flag, value) match {
      case (_: vm.Variable, _: vm.Variable) => sys.error( "set_prolog_flag/2: flag and value must be given" )
      case (f: Symbol, _) if flags.contains(f) && flags(f).changeable && flags(f).valid(value) => flags(f).value = value
      case _ => sys.error( "set_prolog_flag/2: expected valid changeable flag and value" )
    }

  def current_prolog_flag( vm: VM, pos: IndexedSeq[Reader], flag: Any, value: Any ) =
    flag match {
      case _: vm.Variable =>
        val values = flags.toList

        vm.resatisfyable(
          new (VM => Boolean) {
            var rest = values.tail

            def apply( vm1: VM ): Boolean = {
              rest match {
                case List( (k, v) ) => vm.unify( k, flag ) && vm.unify( v.value, value )
                case (k, v) :: t =>
                  rest = t
                  vm.resatisfyable( this )
                  vm.unify( k, flag ) && vm.unify( v.value, value )
              }
            }
          }
        )
        vm.unify( values.head._1, flag ) && vm.unify( values.head._2.value, value )
      case a: Symbol if flags contains a => vm.unify( flags(a).value, value )
      case _ => sys.error( "expected valid flag or variable" )
    }
}
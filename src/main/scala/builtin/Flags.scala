package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{Structure, VM, instantiationError, typeError}

import scala.collection.mutable


object Flags {

  val flags = new mutable.HashMap[Symbol, Any]

  def get_flag( vm: VM, pos: IndexedSeq[Reader], key: Any, value: Any ) =
    key match {
      case _: vm.Variable => instantiationError( pos(0), "key must be given", 'get_flag, 2 )
      case k: Symbol =>
        flags get k match {
          case None =>
            flags(k) = 0
            vm.unify( 0, value )
          case Some( v ) => vm.unify( v, value )
        }
      case _ => typeError( pos(0), "key must be an atom", 'atom, key, 'get_flag, 2 )
    }

  def set_flag( vm: VM, pos: IndexedSeq[Reader], key: Any, value: Any ) =
    (key, value) match {
      case (_: vm.Variable, _) => instantiationError( pos(0), "key must be given", 'set_flag, 2 )
      case (_, _: vm.Variable) => instantiationError( pos(1), "value must be given", 'set_flag, 2 )
      case (k: Symbol, v: Any) if v.isInstanceOf[Number] || v.isInstanceOf[Symbol] =>
        flags(k) = v
        true
      case (_, v: Any) if v.isInstanceOf[Number] || v.isInstanceOf[Symbol] => typeError( pos(0), "key must be an atom", 'atom, key, 'set_flag, 2 )
      case (_: Symbol, _) => typeError( pos(1), "value must be a number or an atom", 'number, key, 'set_flag, 2 )
    }

  def flag( vm: VM, pos: IndexedSeq[Reader], key: Any, old: Any, value: Any ) =
    (key, value) match {
      case (_: vm.Variable, _) => instantiationError( pos(0), "key must be given", 'flag, 3 )
      case (_, _: vm.Variable) => instantiationError( pos(2), "value must be given", 'flag, 3 )
      case (k: Symbol, s: Structure) =>
        if (vm.unify( flags.getOrElse(k, 0), old )) {
          flags(k) = vm.eval(s)
          true
        } else
          false
      case (k: Symbol, v: Any) if v.isInstanceOf[Number] || v.isInstanceOf[Symbol] =>
        if (vm.unify( flags.getOrElse(k, 0), old )) {
          flags(k) = v
          true
        } else
          false
      case (_, v: Any) if v.isInstanceOf[Number] || v.isInstanceOf[Symbol] => typeError( pos(0), "key must be an atom", 'atom, key, 'flag, 3 )
      case (_: Symbol, _) => typeError( pos(2), "value must be a number or an atom", 'number, key, 'flag, 3 )
    }

}
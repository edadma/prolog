package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.prolog.VM


object AtomManipulation {

  def atom_length( vm: VM, atom: Any, length: Any ) =
    atom match {
      case _: vm.Variable => sys.error( "atom_length: atom must be given" )
      case Symbol( a ) => vm.unify( a.length, length )
      case x => sys.error( s"atom_length: expected atom: $x" )
    }

  def char_code( vm: VM, char: Any, code: Any ) =
    (char, code) match {
      case (v: vm.Variable, ch: Int) => v bind Symbol( ch.toChar.toString )
      case (Symbol( a ), _: vm.Variable | _: Int) if a.length == 1 => vm.unify( a.head.toInt, code )
      case _ => sys.error( "char_code: expected one character atom or code" )
    }

}

package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.prolog.VM


object AtomManipulation {

  def atom_length( vm: VM, atom: Any, length: Any ) =
    atom match {
      case _: vm.Variable => sys.error( "atom_length: atom must be given" )
      case Symbol( a ) => vm.unify( a.length, length )
      case x => sys.error( s"atom_length: expected atom: $x" )
    }

}

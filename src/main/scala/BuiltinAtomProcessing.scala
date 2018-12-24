package xyz.hyperreal.prolog


object BuiltinAtomProcessing {

  def atom_length( vm: VM, atom: Any, length: Any ) =
    atom match {
      case vm.Variable => sys.error( "atom_length: atom must be given" )
      case a: Symbol => vm.unify( a, length )
      case x => sys.error( "atom_length: expected atom: $x" )
    }

}
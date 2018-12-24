package xyz.hyperreal.prolog


object BuiltinStringManipulation {

  def string_length( vm: VM, string: Any, length: Any ) =
    string match {
      case _: vm.Variable => sys.error( "string_length: string must be given" )
      case s: String => vm.unify( s.length, length )
      case x => sys.error( s"string_length: expected string: $x" )
    }

  def atom_string( vm: VM, atom: Any, string: Any ) =
    atom match {
      case _: vm.Variable =>
        string match {
          case _: vm.Variable => sys.error( "atom_string: string must be given" )
          case s: String => vm.unify( Symbol(s), atom )
          case x => sys.error( s"atom_string: expected string: $x" )
        }
      case Symbol( a ) => vm.unify( a, string )
      case x => sys.error( s"atom_string: expected atom: $x" )
    }

  def string_codes( vm: VM, string: Any, codes: Any ) =
    string match {
      case _: vm.Variable =>
        codes match {
          case _: vm.Variable => sys.error( "string_codes: string must be given" )
          case s: Structure =>
            list2array( s ) match {
              case Some( a ) =>
                val charcodes =
                  a map {
                    case n: Int if n.isValidChar => n.toChar
                    case _ => sys.error( s"string_codes: expected list of character codes: $a" )
                  }
                vm.unify( new String(charcodes), codes )
              case None => sys.error( s"string_codes: expected list of character codes: $s" )
            }
          case x => sys.error( s"string_codes: expected list of character codes: $x" )
        }
      case s: String => vm.unify( array2list(s.toArray), codes )
      case x => sys.error( s"string_codes: expected string: $x" )
    }

}
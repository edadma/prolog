package xyz.hyperreal.prolog.builtin

import java.io.BufferedReader

import xyz.hyperreal.prolog.{Structure, VM, array2list, list2array}


object StringManipulation {

  def count( vm: VM, limit: Any, i: Any ) =
    limit match {
      case 1 => vm.unify( 1, i )
      case n: Int if n > 1 =>
        vm.resatisfyable(
          new (VM => Boolean) {
            var current = 1

            override def apply( v1: VM ): Boolean = {
              current += 1

              if (current == n)
                vm.unify( n, i )
              else {
                vm.resatisfyable( this )
                vm.unify( current, i )
              }
            }
          }
        )

        vm.unify( 1, i )
      case _ => false
    }

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

  val intRegex = """((?:-|\+)?[0-9]+)"""r
  val floatRegex = """([+-]?(?:[0-9]*\.[0-9]+(?:[Ee][+-]?[0-9]+)?|[0-9]+[Ee][+-]?[0-9]+))"""r

  def number_string( vm: VM, number: Any, string: Any ) =
    number match {
      case _: vm.Variable =>
        string match {
          case _: vm.Variable => sys.error( "number_string: string must be given" )
          case s: String =>
            val n =
              s match {
                case intRegex( v ) => v.toInt
                case floatRegex( v ) => v.toDouble
              }

            vm.unify( n, number )
          case x => sys.error( s"number_string: expected string: $x" )
        }
      case n: Number => vm.unify( n.toString, string )
      case x => sys.error( s"number_string: expected a number: $x" )
    }

  def string_chars( vm: VM, string: Any, chars: Any ) =
    string match {
      case _: vm.Variable =>
        chars match {
          case _: vm.Variable => sys.error( "string_chars: string must be given" )
          case s: Structure =>
            list2array( s ) match {
              case Some( a ) =>
                val charatoms =
                  a map {
                    case Symbol( c ) if c.length == 1 => c.head
                    case _ => sys.error( s"string_chars: expected list of characters: $a" )
                  }
                vm.unify( new String(charatoms), string )
              case None => sys.error( s"string_chars: expected list of character codes: $s" )
            }
          case x => sys.error( s"string_chars: expected list of character codes: $x" )
        }
      case s: String => vm.unify( array2list(s.toArray map (c => Symbol(c.toString))), chars )
      case x => sys.error( s"string_chars: expected string: $x" )
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
                vm.unify( new String(charcodes), string )
              case None => sys.error( s"string_codes: expected list of character codes: $s" )
            }
          case x => sys.error( s"string_codes: expected list of character codes: $x" )
        }
      case s: String => vm.unify( array2list(s.toArray map (_.toInt)), codes )
      case x => sys.error( s"string_codes: expected string: $x" )
    }

  def string_upper( vm: VM, string: Any, upper: Any ) =
    string match {
      case _: vm.Variable => sys.error( "string_upper: string must be given" )
      case s: String => vm.unify( s.toUpperCase, upper )
      case x => sys.error( s"string_upper: expected string: $x" )
    }

  def string_lower( vm: VM, string: Any, lower: Any ) =
    string match {
      case _: vm.Variable => sys.error( "string_lower: string must be given" )
      case s: String => vm.unify( s.toLowerCase, lower )
      case x => sys.error( s"string_lower: expected string: $x" )
    }

  def read_string( vm: VM, stream: Any, string: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "read_string: input stream must be given" )
      case in: BufferedReader =>
        val line = if (in == Console.in && TermIO.repl != null) Console.withIn( TermIO.repl ){ io.StdIn.readLine } else in.readLine

        vm.unify( line, string )
      case x => sys.error( s"read_string: expected input stream" )
    }

}
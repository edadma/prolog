package xyz.hyperreal.prolog.builtin

import java.io.BufferedReader

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{Structure, VM, array2list, list2array}


object StringManipulation {

//  def count( vm: VM, pos: IndexedSeq[Reader], limit: Any, i: Any ) =
//    limit match {
//      case 1 => vm.unify( 1, i )
//      case n: Int if n > 1 =>
//        vm.resatisfyable(
//          new (VM => Boolean) {
//            var current = 1
//
//            def apply( v1: VM ): Boolean = {
//              current += 1
//
//              if (current == n)
//                vm.unify( n, i )
//              else {
//                vm.resatisfyable( this )
//                vm.unify( current, i )
//              }
//            }
//          }
//        )
//
//        vm.unify( 1, i )
//      case _ => false
//    }

  def string_length( vm: VM, pos: IndexedSeq[Reader], string: Any, length: Any ) =
    string match {
      case _: vm.Variable => sys.error( "string_length: string must be given" )
      case s: String => vm.unify( s.length, length )
      case x => sys.error( s"string_length: expected string: $x" )
    }

  def atom_string( vm: VM, pos: IndexedSeq[Reader], atom: Any, string: Any ) =
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

  def number_string( vm: VM, pos: IndexedSeq[Reader], number: Any, string: Any ) =
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

  def string_chars( vm: VM, pos: IndexedSeq[Reader], string: Any, chars: Any ) =
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

  def string_codes( vm: VM, pos: IndexedSeq[Reader], string: Any, codes: Any ) =
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

  def string_concat( vm: VM, pos: IndexedSeq[Reader], s1: Any, s2: Any, s3: Any ) =
    (s1, s2, s3) match {
      case (b1: String, b2: String, v3: vm.Variable ) => v3 bind (b1 + b2)
      case (b1: String, v2: vm.Variable, b3: String) =>
        if (b3.startsWith( b1 ))
          v2 bind (b3 substring b1.length)
        else
          false
      case (v1: vm.Variable, b2: String, b3: String) =>
        if (b3.endsWith( b2 ))
          v1 bind (b3 substring (0, b3.length - b2.length))
        else
          false
      case (v1: vm.Variable, v2: vm.Variable, "") =>
        v1 bind ""
        v2 bind ""
      case (v1: vm.Variable, v2: vm.Variable, b3: String) =>
        vm.resatisfyable(
          new (VM => Boolean) {
            var idx = 1

            def apply( v: VM ): Boolean = {
              if (idx < b3.length)
                vm.resatisfyable( this )

              v1 bind b3.substring( 0, idx )
              v2 bind b3.substring( idx )
              idx += 1
              true
            }
          }
        )
        v1 bind ""
        v2 bind b3
      case _ => sys.error( s"string_concat: expected three strings: $s1, $s2, $s3" )
    }

  def sub_string( vm: VM, pos: IndexedSeq[Reader], string: Any, before: Any, length: Any, after: Any, sub: Any ) =
    string match {
      case str: String =>
        (before, length, after, sub) match {
          case (b: Int, l: Int, _, _) if 0 <= b && b < str.length && 0 < l && l <= str.length =>
            vm.unify( str.length - b - l, after ) && vm.unify( str.substring(b, b + l), sub )
          case (b: Int, l: Int, _, _) => sys.error( s"sub_string: before or length out of range" )
          case (_, l: Int, a: Int, _) =>
            vm.unify( str.length - l - a, before ) && vm.unify( str.substring(str.length - l - a, str.length - a), sub )
          case (b: Int, _, _, s: String) =>
            vm.unify( str.length - b - s.length, after ) && vm.unify( str.substring(b, b + s.length), sub )
          case (_, _, a: Int, s: String) =>
            vm.unify( str.length - s.length - a, before ) && vm.unify( str.substring(str.length - s.length - a, str.length - a), sub )
          case (b: Int, _, a: Int, _) =>
            val len = str.length - b - a

            vm.unify( len, length ) && vm.unify( str.substring(b, b + len), sub )
//          case (b: Int, _, _, _) =>
          case (_, l: Int, _, _) =>
            if (l < str.length) {
              vm.resatisfyable(
                new (VM => Boolean) {
                  var idx = 1

                  def apply( v: VM ): Boolean = {
                    if (idx + l < str.length)
                      vm.resatisfyable( this )

                    val cur = idx

                    idx += 1
                    vm.unify( cur, before ) && vm.unify( str.length - cur - l, after ) && vm.unify( str.substring(cur, cur + l), sub )
                  }
                }

              )
              vm.unify( 0, before ) && vm.unify( str.length - l, after ) && vm.unify( str.substring(0, l), sub )
            } else if (l == str.length)
              vm.unify( 0, before ) && vm.unify( 0, after ) && vm.unify( str, sub )
            else
              false
  //        case (_, _, a: Int, _) =>
          case (_, _, _, s: String) =>
            str indexOf s match {
              case -1 => false
              case idx =>
                str.indexOf( s, idx + 1 ) match {
                  case -1 =>
                    vm.unify( idx, before ) && vm.unify( s.length, length ) && vm.unify( str.length - (idx + s.length), after )
                  case idx1 =>
                    vm.resatisfyable(
                      new (VM => Boolean) {
                        var next = idx1

                        def apply( v1: VM ): Boolean = {
                          str.indexOf( s, next + 1 ) match {
                            case -1 =>
                              vm.unify( next, before ) && vm.unify( s.length, length ) && vm.unify( str.length - (next + s.length), after )
                            case idx3 =>
                              val idx4 = next

                              next = idx3

                              vm.resatisfyable( this )
                              vm.unify( idx4, before ) && vm.unify( s.length, length ) && vm.unify( str.length - (idx4 + s.length), after )
                          }
                        }
                      }
                    )
                    vm.unify( idx, before ) && vm.unify( s.length, length ) && vm.unify( str.length - (idx + s.length), after )
                }
            }
//          case (_: vm.Variable, _: vm.Variable, _: vm.Variable, _: vm.Variable) =>
          case _ => sys.error( s"sub_string: invalid arguments" )
        }
      case _ => sys.error( s"sub_string: string must be given" )
    }

  def string_upper( vm: VM, pos: IndexedSeq[Reader], string: Any, upper: Any ) =
    string match {
      case _: vm.Variable => sys.error( "string_upper: string must be given" )
      case s: String => vm.unify( s.toUpperCase, upper )
      case x => sys.error( s"string_upper: expected string: $x" )
    }

  def string_lower( vm: VM, pos: IndexedSeq[Reader], string: Any, lower: Any ) =
    string match {
      case _: vm.Variable => sys.error( "string_lower: string must be given" )
      case s: String => vm.unify( s.toLowerCase, lower )
      case x => sys.error( s"string_lower: expected string: $x" )
    }

  def string_code( vm: VM, pos: IndexedSeq[Reader], index: Any, string: Any, code: Any ) =
    string match {
      case _: vm.Variable => sys.error( "string_code: string must be given" )
      case s: String =>
        index match {
          case _: vm.Variable =>
            code match {
              case _: vm.Variable => sys.error( "string_code: code must be given if index is a variable" )
              case c: Int =>
                s indexOf c match {
                  case -1 => false
                  case first =>
                    s.indexOf( c, first + 1 ) match {
                      case -1 => vm.unify( first, index )
                      case second =>
                        vm.resatisfyable(
                          new (VM => Boolean) {
                            var next = second

                            def apply( v1: VM ): Boolean = {
                              s.indexOf( c, next + 1 ) match {
                                case -1 => vm.unify( next, index )
                                case after =>
                                  vm.resatisfyable( this )

                                  val n = next

                                  next = after
                                  vm.unify( n, index )
                              }
                            }
                          }
                        )

                        vm.unify( first, index )
                    }
                }
              case _ => sys.error( "string_code: code must be integer" )
            }
          case idx: Int =>
            if (0 <= idx && idx < s.length)
              vm.unify( s(idx).toInt, code )
            else
              false
          case _ => sys.error( "string_code: index must be integer" )
        }
    }

  def read_string( vm: VM, pos: IndexedSeq[Reader], stream: Any, string: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "read_string: input stream must be given" )
      case in: BufferedReader =>
        val line = if (in == Console.in && TermIO.repl != null) Console.withIn( TermIO.repl ){ io.StdIn.readLine } else in.readLine

        vm.unify( line, string )
      case x => sys.error( s"read_string: expected input stream" )
    }

}
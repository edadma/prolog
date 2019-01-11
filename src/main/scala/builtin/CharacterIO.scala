package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.prolog.{SinkStream, SourceStream, VM}


object CharacterIO {

  def put_char( vm: VM, s: Any, char: Any ) =
    (s, char) match {
      case (p: SinkStream, Symbol( c )) if c.length == 1 =>
        p print c.head
        true
      case (p: SinkStream, Symbol( c )) => sys.error( s"expected one character atom: $c" )
      case _ => sys.error( "put_char: expected an output stream and a one character atom")
    }

  def get_char( vm: VM, s: Any, char: Any ) =
    s match {
      case in: SourceStream =>
        in read match {
          case -1 => vm.unify( 'end_of_file, char )
          case c => vm.unify( Symbol(c.toChar.toString), char )
        }
      case _ => sys.error( "get_char: expected character input stream" )
    }

}
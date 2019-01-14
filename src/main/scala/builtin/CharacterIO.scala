package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{SinkStream, SourceStream, VM, domainError, instantiationError, typeError}


object CharacterIO {

  def put_char( vm: VM, pos: IndexedSeq[Reader], s: Any, char: Any ) =
    (Streams(s), char) match {
      case (p: SinkStream, Symbol( c )) if c.length == 1 =>
        p print c.head
        true
//      case (_: SourceStream, _) => domainError( pos(0), "expected output stream", '
      case (_: SinkStream, _: Symbol) => typeError( pos(1), "expected one character atom", 'character, char, 'put_char, 2 )
      case (_: vm.Variable, _) => instantiationError( pos(0), "output stream must be given", 'put_char, 2 )
      case (_, _: vm.Variable) => instantiationError( pos(1), "one character atom must be given", 'put_char, 2 )
//      case (_, _: Symbol) =>
//      case _ => problem( pos(0), "put_char: expected an output stream and a one character atom" )
    }

  def get_char( vm: VM, pos: IndexedSeq[Reader], s: Any, char: Any ) =
    s match {
      case in: SourceStream =>
        in read match {
          case -1 => vm.unify( 'end_of_file, char )
          case c => vm.unify( Symbol(c.toChar.toString), char )
        }
      case _ => sys.error( "get_char: expected character input stream" )
    }

}
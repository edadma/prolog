package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{SinkStream, SourceStream, TextSourceStream, VM, domainError, instantiationError, permissionError, typeError}


object CharacterIO {

  def put_char( vm: VM, pos: IndexedSeq[Reader], s: Any, char: Any ) =
    (Streams(s), char) match {
      case (_: vm.Variable, _) => instantiationError( pos(0), "output stream must be given", "put_char", 2 )
      case (_, _: vm.Variable) => instantiationError( pos(1), "one character atom must be given", "put_char", 2 )
      case (p: SinkStream, Symbol( c )) if c.length == 1 && p.open =>
        p print c.head
        true
      case (_: SinkStream, Symbol( c )) if c.length == 1 => typeError( pos(0), "output stream is closed", "stream", char, "put_char", 2 )
      case (_: SourceStream, _) => permissionError( pos(0), "expected output stream", "output", "stream", s, "put_char", 2 )
      case (_: SinkStream, _) => typeError( pos(1), "expected one character atom", "character", char, "put_char", 2 )
      case _ => domainError( pos(0), "expected output stream", "stream_or_alias", s, "put_char", 2 )
    }

  def get_char( vm: VM, pos: IndexedSeq[Reader], s: Any, char: Any ) =
    s match {
      case _: vm.Variable => instantiationError( pos(0), "input stream must be given", "get_char", 2 )
      case _: SinkStream => permissionError( pos(0), "expected character input stream", "input", "stream", s, "get_char", 2 )
      case in: TextSourceStream =>
        in read match {
          case -1 => vm.unify( Symbol("end_of_file"), char )
          case c => vm.unify( Symbol(c.toChar.toString), char )
        }
      case _: SourceStream => permissionError( pos(0), "expected character input stream", "input", "binary_stream", s, "get_char", 2 )
      case _ => domainError( pos(0), "expected character input stream", "stream_or_alias", s, "get_char", 2 )
    }

}
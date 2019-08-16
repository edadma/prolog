package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{BinarySinkStream, BinarySourceStream, SinkStream, SourceStream, TextSinkStream, VM, domainError, instantiationError, permissionError, typeError}


object BinaryIO {

  def put_byte( vm: VM, pos: IndexedSeq[Reader], s: Any, byte: Any ) =
    (s, byte) match {
      case (_: vm.Variable, _) => instantiationError( pos(0), "output stream must be given", "put_byte", 2 )
      case (_, _: vm.Variable) => instantiationError( pos(1), "integer must be given", "put_byte", 2 )
      case (p: BinarySinkStream, b: Int) if b.isValidByte && p.open =>
        p write b
        true
      case (_: SinkStream, b: Int) if b.isValidByte => typeError( pos(0), "output stream is closed", "stream", byte, "put_byte", 2 )
      case (_: BinarySinkStream, _: Int) => permissionError( pos(0), "expected binary output stream", "output", "text_stream", s, "put_byte", 2 )
      case (_: SourceStream, _) => permissionError( pos(0), "expected output stream", "output", "stream", s, "put_byte", 2 )
      case _ => domainError( pos(0), "expected output stream", "stream_or_alias", s, "put_byte", 2 )
    }

  def get_byte( vm: VM, pos: IndexedSeq[Reader], s: Any, byte: Any ) =
    s match {
      case _: vm.Variable => instantiationError( pos(0), "input stream must be given", "get_byte", 2 )
      case _: SinkStream => permissionError( pos(0), "expected input stream", "input", "stream", s, "get_byte", 2 )
      case in: BinarySourceStream =>
        in read match {
          case -1 => vm.unify( Symbol("end_of_file"), byte )
          case c => vm.unify( c, byte )
        }
      case _: SourceStream => permissionError( pos(0), "expected binary input stream", "input", "text_stream", s, "get_byte", 2 )
      case _ => domainError( pos(0), "expected binary input stream", "stream_or_alias", s, "get_byte", 2 )
    }

}
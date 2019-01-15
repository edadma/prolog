package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{BinarySinkStream, BinarySourceStream, SinkStream, TextSinkStream, VM, instantiationError, typeError}


object BinaryIO {

  def put_byte( vm: VM, pos: IndexedSeq[Reader], s: Any, byte: Any ) =
    (s, byte) match {
      case (_: vm.Variable, _) => instantiationError( pos(0), "output stream must be given", 'put_byte, 2 )
      case (_, _: vm.Variable) => instantiationError( pos(1), "integer must be given", 'put_byte, 2 )
      case (p: BinarySinkStream, b: Int) if b.isValidByte && p.open =>
        p write b
        true
      case (_: SinkStream, b: Int) if b.isValidByte => typeError( pos(0), "output stream is closed", 'stream, byte, 'put_char, 2 )
      case (_: TextSinkStream, Symbol( c )) => sys.error( s"expected one character atom: $c" )
      case _ => sys.error( "put_char: expected a binary output stream and an integer (byte)" )
    }

  def get_byte( vm: VM, pos: IndexedSeq[Reader], s: Any, byte: Any ) =
    s match {
      case in: BinarySourceStream =>
        in read match {
          case -1 => vm.unify( 'end_of_file, byte )
          case c => vm.unify( Symbol(c.toChar.toString), byte )
        }
      case _ => sys.error( "get_char: expected binary input stream" )
    }

}
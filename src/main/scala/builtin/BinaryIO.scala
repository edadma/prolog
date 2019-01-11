package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.prolog.{BinarySinkStream, BinarySourceStream, VM}


object BinaryIO {

  def put_byte( vm: VM, s: Any, byte: Any ) =
    (s, byte) match {
      case (p: BinarySinkStream, b: Int) if b.isValidByte =>
        p write b
        true
      case (p: BinarySinkStream, Symbol( c )) => sys.error( s"expected one character atom: $c" )
      case _ => sys.error( "put_char: expected a binary output stream and an integer (byte)" )
    }

  def get_byte( vm: VM, s: Any, byte: Any ) =
    s match {
      case in: BinarySourceStream =>
        in read match {
          case -1 => vm.unify( 'end_of_file, byte )
          case c => vm.unify( Symbol(c.toChar.toString), byte )
        }
      case _ => sys.error( "get_char: expected binary input stream" )
    }

}
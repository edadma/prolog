package xyz.hyperreal.prolog

import java.io.{FileInputStream, PrintStream, BufferedReader}


trait Stream {
  def file_name: Option[String]
  def mode: Symbol
  def alias: Option[Symbol]
  def position: BigInt
  def atEnd: Boolean
  def typ: Symbol

  def close
}

trait SourceStream extends Stream {

  def read: Int

  def readLine: String

}

trait SinkStream extends Stream {

  def write( b: Int )

  def println( a: Any )

}

abstract class TextSourceStream( val reader: BufferedReader ) extends SourceStream {

  val mode = 'read
  val position = 0

  def atEnd =
    if (reader.markSupported) {
      reader.mark( 1 )

      val c = reader.read

      reader.reset
      c == -1
    } else
      sys.error( "not supported" )

  val typ = 'text

  def close = reader.close

  def read = reader.read

  def readLine = reader.readLine

}

abstract class BinarySinkStream( val out: PrintStream, val append: Boolean ) extends SinkStream {

  val mode = if (append) 'append else 'write
  val position = 0

  def atEnd = false

  val typ = 'binary

  def close = out.close

  def write( b: Int ) = out.write( b )

  def println( a: Any ) = out.println( a )

}

class StandardInput extends TextSourceStream( Console.in ) {

  val file_name = None

  val alias = Some( 'user_input )

  override def close = sys.error( "attempt to close standard input" )

}

class StandardOutput extends BinarySinkStream( Console.out, false ) {

  val file_name = None

  val alias = Some( 'user_output )

  override def close = sys.error( "attempt to close standard output" )

}
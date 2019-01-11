package xyz.hyperreal.prolog

import java.io.{BufferedReader, InputStream, PrintStream, PrintWriter}


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

  def print( a: Any )

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

abstract class BinarySourceStream( val input: InputStream ) extends SourceStream {

  val mode = 'read
  val position = 0

  def atEnd =
    if (input.markSupported) {
      input.mark( 1 )

      val c = input.read

      input.reset
      c == -1
    } else
      sys.error( "not supported" )

  val typ = 'binary

  def close = input.close

  def read = input.read

  def readLine = sys.error( "BinarySourceStream.readLine: not supported" )

}

abstract class TextSinkStream( val out: PrintWriter, val append: Boolean ) extends SinkStream {

  val mode = if (append) 'append else 'write
  val position = 0

  def atEnd = false

  val typ = 'text

  def close = out.close

  def write( b: Int ) = out.write( b )

  def print( a: Any ) = out.print( a )

  def println( a: Any ) = out.println( a )

}

abstract class BinarySinkStream( val out: PrintStream, val append: Boolean ) extends SinkStream {

  val mode = if (append) 'append else 'write
  val position = 0

  def atEnd = false

  val typ = 'binary

  def close = out.close

  def write( b: Int ) = out.write( b )

  def print( a: Any ) = out.print( a )

  def println( a: Any ) = out.println( a )

}

object ConsoleInput extends TextSourceStream( Console.in ) {

  val file_name = None

  val alias = Some( 'user_input )

  override def close = sys.error( "attempt to close standard input" )

}

object ConsoleOutput extends BinarySinkStream( Console.out, false ) {

  val file_name = None

  val alias = Some( 'user_output )

  override def close = sys.error( "attempt to close standard output" )

}

object SystemInput extends BinarySourceStream( System.in ) {

  val file_name = None

  val alias = None

  override def close = sys.error( "attempt to close standard input" )

}
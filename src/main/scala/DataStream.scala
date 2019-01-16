package xyz.hyperreal.prolog

import java.io.{BufferedReader, InputStream, PrintStream, PrintWriter}


abstract class DataStream {

  protected var _open = true

  def open = _open

  def file_name: Option[String]

  def mode: Symbol

  def input: Boolean

  def output: Boolean

  def alias: Option[Symbol]

  def position: BigInt

  def atEnd: Boolean

  def typ: Symbol

  def close = _open = false

}

abstract class SourceStream extends DataStream {

  val input = true
  val output = false

  def read: Int

  def readLine: String

}

abstract class SinkStream extends DataStream {

  val input = false
  val output = true

  def flush

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

  override def close = {
    super.close
    reader.close
  }

  def read = reader.read

  def readLine = reader.readLine

}

abstract class BinarySourceStream( val in: InputStream ) extends SourceStream {

  val mode = 'read
  val position = 0

  def atEnd =
    if (in.markSupported) {
      in.mark( 1 )

      val c = in.read

      in.reset
      c == -1
    } else
      sys.error( "not supported" )

  val typ = 'binary

  override def close = {
    super.close
    in.close
  }

  def read = in.read

  def readLine = sys.error( "BinarySourceStream.readLine: not supported" )

}

abstract class TextSinkStream( val out: PrintWriter, val append: Boolean ) extends SinkStream {

  val mode = if (append) 'append else 'write
  val position = 0

  def atEnd = false

  val typ = 'text

  def flush = out.flush

  override def close = {
    super.close
    out.close
  }

  def write( b: Int ) = out.write( b )

  def print( a: Any ) = out.print( a )

  def println( a: Any ) = out.println( a )

}

abstract class BinarySinkStream( val out: PrintStream, val append: Boolean ) extends SinkStream {

  val mode = if (append) 'append else 'write
  val position = 0

  def atEnd = false

  val typ = 'binary

  def flush = out.flush

  override def close = {
    super.close
    out.close
  }

  def write( b: Int ) = out.write( b )

  def print( a: Any ) = out.print( a )

  def println( a: Any ) = out.println( a )

}

object ConsoleInput extends TextSourceStream( Console.in ) {

  val file_name = None

  val alias = Some( 'user_input )

  override def close = sys.error( "attempt to close standard input" )

  override def toString: String = "[stream console input]"

}

object ConsoleOutput extends BinarySinkStream( Console.out, false ) {

  val file_name = None

  val alias = Some( 'user_output )

  override def flush {}

  override def close = sys.error( "attempt to close standard output" )

  override def toString: String = "[stream console output]"

}

object SystemInput extends BinarySourceStream( System.in ) {

  val file_name = None

  val alias = Some( 'stdin )

  override def close = sys.error( "attempt to close standard input" )

  override def toString: String = "[stream system input]"

}
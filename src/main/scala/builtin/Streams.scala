package xyz.hyperreal.prolog.builtin

import java.io.{BufferedReader, FileReader, FileWriter, PrintWriter}

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{DataStream, SinkStream, SourceStream, SystemInput, SystemOutput, TextSinkStream, TextSourceStream, UserInput, UserOutput, VM, list2array}

import scala.collection.mutable


object Streams {

  var input: SourceStream = UserInput
  var output: SinkStream = UserOutput
  val aliases =
    mutable.HashMap[Symbol, DataStream] (
      Symbol("user_input") -> UserInput,
      Symbol("user_output") -> UserOutput,
      Symbol("stdin") -> SystemInput,
      Symbol("stdout") -> SystemOutput
    )

  def apply( s: Any ) =
    s match {
      case stream: DataStream => stream
      case alias: Symbol =>
        aliases get alias match {
          case Some( stream1: DataStream ) => stream1
          case _ => alias
        }
      case _ => s
    }

  def current_input( vm: VM, pos: IndexedSeq[Reader], stream: Any ) = vm.unify( stream, input )

  def current_output( vm: VM, pos: IndexedSeq[Reader], stream: Any ) = vm.unify( stream, output )

  def set_input( vm: VM, pos: IndexedSeq[Reader], stream: Any ) =
    Streams( stream ) match {
      case _: vm.Variable => sys.error( "set_input: input stream must be given" )
      case s: SourceStream =>
        input = s
        true
      case _ => sys.error( "set_input: stream is not a source stream" )
    }

  def set_output( vm: VM, pos: IndexedSeq[Reader], stream: Any ) =
    Streams( stream ) match {
      case _: vm.Variable => sys.error( "set_output: output stream must be given" )
      case s: SinkStream =>
        output = s
        true
      case _ => sys.error( "set_output: stream is not a sink stream" )
    }

  def open( vm: VM, pos: IndexedSeq[Reader], file: Any, mode: Any, stream: Any, options: Any ) =
    (file, mode, stream, list2array(options) map (_.toList)) match {
      case (f: String, m@(Symbol("read")|Symbol("write")|Symbol("append")), s: vm.Variable, o) =>
        val s1 =
          m match {
            case Symbol("read") =>
              new TextSourceStream( new BufferedReader(new FileReader(f)) ) {
                val file_name = Some( f )
                val alias = None
              }
            case Symbol("write") =>
              new TextSinkStream( new PrintWriter(f), false ) {
                val file_name = Some( f )
                val alias = None
              }
            case Symbol("append") =>
              new TextSinkStream( new PrintWriter( new FileWriter(f, true) ), true ) {
                val file_name = Some( f )
                val alias = None
              }
          }

        s bind s1
      case _ => sys.error( "open/4: invalid arguments" )
    }

  def close( vm: VM, pos: IndexedSeq[Reader], stream: Any, options: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "close/2: stream must be given" )
      case s: DataStream => s.close
      case _ => sys.error( "close/2: invalid arguments" )
    }

  def flush_output( vm: VM, pos: IndexedSeq[Reader], stream: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "flush_output/1: output stream must be given" )
      case s: SinkStream => s.flush
      case _ => sys.error( "flush_output/1: expected output stream" )
    }

}
package xyz.hyperreal.prolog.builtin

import java.io.{BufferedReader, FileReader, FileWriter, PrintWriter}

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{ConsoleInput, ConsoleOutput, DataStream, SinkStream, SourceStream, TextSinkStream, TextSourceStream, VM, list2array}

import scala.collection.mutable


object StreamSelection {

  var input: SourceStream = ConsoleInput
  var output: SinkStream = ConsoleOutput
  val aliases = new mutable.HashMap[Symbol, DataStream]

  def current_input( vm: VM, pos: IndexedSeq[Reader], stream: Any ) = vm.unify( stream, input )

  def current_output( vm: VM, pos: IndexedSeq[Reader], stream: Any ) = vm.unify( stream, output )

  def set_input( vm: VM, pos: IndexedSeq[Reader], stream: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "set_input: stream is a variable" )
      case s: SourceStream =>
        input = s
        true
      case a: Symbol if aliases.contains( a ) && aliases(a).input =>
        input = aliases(a).asInstanceOf[SourceStream]
        true
      case _ => sys.error( "set_input: stream is not a source stream" )
    }

  def set_output( vm: VM, pos: IndexedSeq[Reader], stream: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "set_output: stream is a variable" )
      case s: SinkStream =>
        output = s
        true
      case a: Symbol if aliases.contains( a ) && aliases(a).output =>
        output = aliases(a).asInstanceOf[SinkStream]
        true
      case _ => sys.error( "set_output: stream is not a sink stream" )
    }

  def open( vm: VM, pos: IndexedSeq[Reader], file: Any, mode: Any, stream: Any, options: Any ) =
    (file, mode, stream, list2array(options) map (_.toList)) match {
      case (f: String, m@('read|'write|'append), s: vm.Variable, o) =>
        val s1 =
          m match {
            case 'read =>
              new TextSourceStream( new BufferedReader(new FileReader(f)) ) {
                val file_name = Some( f )
                val alias = None
              }
            case 'write =>
              new TextSinkStream( new PrintWriter(f), false ) {
                val file_name = Some( f )
                val alias = None
              }
            case 'append =>
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
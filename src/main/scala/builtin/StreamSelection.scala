package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.prolog.{list2array, ConsoleInput, ConsoleOutput, SinkStream, SourceStream, DataStream, VM}

import scala.collection.mutable


object StreamSelection {

  var input: SourceStream = ConsoleInput
  var output: SinkStream = ConsoleOutput
  val aliases = new mutable.HashMap[Symbol, DataStream]

  def current_input( vm: VM, stream: Any ) = vm.unify( stream, input )

  def current_output( vm: VM, stream: Any ) = vm.unify( stream, output )

  def set_input( vm: VM, stream: Any ) =
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

  def set_output( vm: VM, stream: Any ) =
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

//  def open( vm: VM, file: Any, mode: Any, stream: Any, options: Any ) =
//    (file, mode, stream, list2array(options) map (_.toList)) match {
//      case (file1: String, mode1: Symbol, _: vm.Variable, options1) =>
//        val
//    }

}
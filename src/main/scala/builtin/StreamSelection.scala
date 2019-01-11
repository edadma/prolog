package xyz.hyperreal.prolog.builtin

import java.io.{BufferedReader, PrintStream}

import xyz.hyperreal.prolog.{SinkStream, SourceStream, StandardInput, StandardOutput, VM}


object StreamSelection {

  var input: SourceStream = StandardInput
  var output: SinkStream = StandardOutput

  def current_input( vm: VM, stream: Any ) = vm.unify( stream, input )

  def current_output( vm: VM, stream: Any ) = vm.unify( stream, output )

  def set_input( vm: VM, stream: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "set_input: stream is a variable" )
      case s: SourceStream =>
        input = s
        true
      case _ => sys.error( "set_input: stream is not a source stream" )
    }

  def set_output( vm: VM, stream: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "set_output: stream is a variable" )
      case s: SinkStream =>
        output = s
        true
      case _ => sys.error( "set_output: stream is not a sink stream" )
    }

}
package xyz.hyperreal.prolog.builtin

import java.io.{PrintStream, Reader}

import xyz.hyperreal.prolog.VM


object StreamSelection {

  var input: Reader = Console.in
  var output: PrintStream = Console.out

  def current_input( vm: VM, stream: Any ) = vm.unify( stream, input )

  def current_output( vm: VM, stream: Any ) = vm.unify( stream, output )

  def set_input( vm: VM, stream: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "set_input: stream is a variable" )
      case r: Reader =>
        input = r
        true
      case _ => sys.error( "set_input: stream is not an input stream" )
    }

  def set_output( vm: VM, stream: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "set_output: stream is a variable" )
      case s: PrintStream =>
        output = s
        true
      case _ => sys.error( "set_output: stream is not an output stream" )
    }

}
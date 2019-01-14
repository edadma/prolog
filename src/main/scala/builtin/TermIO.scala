package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.{Reader, StringReader}
import xyz.hyperreal.prolog.{ConsoleInput, Parser, SinkStream, TextSourceStream, VM, display}


object TermIO {

  val repl: java.io.InputStream = null

  def write_term( vm: VM, pos: IndexedSeq[Reader], stream: Any, term: Any, options: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "write_term: stream is a variable" )
      case out: SinkStream =>
        term match {
          case _: vm.Variable => sys.error( "write_term: term is a variable" )
          case data =>
            out.print( display(data) )
            true
        }
    }

  def read_term( vm: VM, pos: IndexedSeq[Reader], stream: Any, term: Any, options: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "read_term: stream is a variable" )
      case in: TextSourceStream =>
        val line = if (in == ConsoleInput && repl != null) Console.withIn( repl ){ io.StdIn.readLine } else in.readLine

        Parser.term( new StringReader(line) ) match {
          case Parser.Match( ast, _ ) => vm.unify( vm.data(ast), term )
          case m: Parser.Mismatch => m.error
        }
    }

}
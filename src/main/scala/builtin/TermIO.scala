package xyz.hyperreal.prolog.builtin

import java.io.{InputStream, PrintStream, BufferedReader}

import xyz.hyperreal.pattern_matcher.StringReader
import xyz.hyperreal.prolog.{Parser, VM, display}


object TermIO {

  val repl: InputStream = null

//  def write( vm: VM, a: Any ) = {
//    print( display(a) )
//    true
//  }

  def write_term( vm: VM, options: Any, term: Any, stream: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "write_term: stream is a variable" )
      case out: PrintStream =>
        term match {
          case _: vm.Variable => sys.error( "write_term: term is a variable" )
          case data =>
            out.print( display(data) )
            true
        }
    }

  def read_term( vm: VM, options: Any, term: Any, stream: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "read_term: stream is a variable" )
      case in: BufferedReader =>
        val line = if (in == Console.in && repl != null) Console.withIn( repl ){ io.StdIn.readLine } else in.readLine

        Parser.term( new StringReader(line) ) match {
          case Parser.Match( ast, _ ) => vm.unify( vm.data(ast), term )
          case m: Parser.Mismatch => m.error
        }
    }


//  def read( vm: VM, term: Any ) = {
//    Parser.term( new StringReader(if (repl eq null) io.StdIn.readLine else ) ) match {
//      case Parser.Match( ast, _ ) => vm.unify( vm.data(ast), term )
//      case m: Parser.Mismatch => m.error
//    }
//  }

}
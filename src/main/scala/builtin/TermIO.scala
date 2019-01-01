package xyz.hyperreal.prolog.builtin

import java.io.{InputStream, PrintStream}

import xyz.hyperreal.pattern_matcher.StringReader
import xyz.hyperreal.prolog.{Parser, VM, display}


object TermIO {

//  val repl: InputStream = null
//
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
            print( display(data) )
            true
        }
    }

//  def read( vm: VM, term: Any ) = {
//    Parser.term( new StringReader(if (repl eq null) io.StdIn.readLine else Console.withIn( repl ){ io.StdIn.readLine }) ) match {
//      case Parser.Match( ast, _ ) => vm.unify( vm.data(ast), term )
//      case m: Parser.Mismatch => m.error
//    }
//  }

}
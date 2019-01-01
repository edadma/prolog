package xyz.hyperreal.prolog.builtin

import java.io.InputStream

import xyz.hyperreal.pattern_matcher.StringReader
import xyz.hyperreal.prolog.{Parser, VM, display}


object TermIO {

  val repl: InputStream = null

  def write( vm: VM, a: Any ) = {
    print( display(a) )
    true
  }

  def read( vm: VM, term: Any ) = {
    Parser.term( new StringReader(if (repl eq null) io.StdIn.readLine else Console.withIn( repl ){ io.StdIn.readLine }) ) match {
      case Parser.Match( ast, _ ) => vm.unify( vm.data(ast), term )
      case m: Parser.Mismatch => m.error
    }
  }

}

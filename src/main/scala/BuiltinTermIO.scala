package xyz.hyperreal.prolog

import java.io.InputStream

import xyz.hyperreal.pattern_matcher.StringReader


object BuiltinTermIO {

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

  def writeln( vm: VM, a: Any ) = {
    println( display(a) )
    true
  }

}
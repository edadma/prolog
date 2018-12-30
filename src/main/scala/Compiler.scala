package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.Reader


object Compiler extends App {

  if (args.length < 1) {
    println( "expected name of source file" )
    sys.exit( 1 )
  }

  Parser.source( Reader.fromFile(args(0) + ".prolog") ) match {
    case Parser.Match( ast, _ ) =>
      val prog = new Program

      Compilation.compile( ast, prog )
      prog.save( args(0) + ".pcc" )
    case m: Parser.Mismatch => m.error
  }

}
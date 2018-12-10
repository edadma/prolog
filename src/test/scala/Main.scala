package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  val code =
    """
      |asdf( a ).
    """.stripMargin
  val query =
    """
      |asdf( X )
    """.stripMargin
  val prog = new Program

  Parser.source( new StringReader(code) ) match {
    case Parser.Match( ast, _ ) =>
      println( ast )
      Compiler.compile( ast, prog )
    case m: Parser.Mismatch => m.error
  }

  Parser.query( new StringReader(query) ) match {
    case Parser.Match( ast, _ ) =>
      println( ast )

      val vm = new VM( prog ) {trace = true}

      println( vm.interp(ast) )
    case m: Parser.Mismatch => m.error
  }

}
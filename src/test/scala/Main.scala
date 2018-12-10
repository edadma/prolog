package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  val code =
    """
      |asdf( a ).
      |asdf( b ).
    """.stripMargin
  val query =
    """
      |asdf( X )
    """.stripMargin
  val prog = new Program

  Parser.source( new StringReader(code) ) match {
    case Parser.Match( ast, _ ) =>
      //println( ast )
      Compiler.compile( ast, prog )
      //prog.print
    case m: Parser.Mismatch => m.error
  }

  Parser.query( new StringReader(query) ) match {
    case Parser.Match( ast, _ ) =>
      //println( ast )

      val vm = new VM( prog ) //{trace = true}

      println( vm.interp(ast) )
      vm.fail
      println( vm.run )
    case m: Parser.Mismatch => m.error
  }

}
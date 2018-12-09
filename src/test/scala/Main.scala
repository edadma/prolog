package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  val input =
    """
      |write( hello )
    """.stripMargin

  Parser.query( new StringReader(input) ) match {
    case Parser.Match( ast, _ ) =>
      println( ast )

      val prog = new Program
      val vm = new VM( prog )

      vm.interp( ast )
    case m: Parser.Mismatch => m.error
  }

}
package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  val code =
    """
      |eats(tony,apples).                            /* "Tony eats apples" */
      |eats(fred,oranges).                           /* "Fred eats oranges" */
      |eats(fred,t_bone_steaks).                     /* "Fred eats T-bone steaks" */
      |eats(john,apples).                            /* "John eats apples" */
      |eats(john,grapefruit).                        /* "John eats grapefruit" */
    """.stripMargin
  val query =
    """
      |eats(W, apples)
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

      println( "-----" )

      vm.interp( ast ) match {
        case Some( r ) =>
          def results( res: Any ): Unit = {
            println( res )
            vm.fail

            vm.run match {
              case Some( r1 ) => results( r1 )
              case None =>
            }
          }

          results( r )
        case None =>
      }

      println( "-----" )
    case m: Parser.Mismatch => m.error
  }

}
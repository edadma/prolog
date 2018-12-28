package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  val code =
    """
       |female( anne ).
       |female( rosie ).
       |female( emma ).
       |female( olivia ).
       |female( mia ).
       |
       |male( randy ).
       |male( don ).
       |male( liam ).
       |male( logan ).
       |male( aiden ).
       |
       |parent( don, randy ).
       |parent( don, anne ).
       |parent( rosie, randy ).
       |parent( rosie, anne ).
       |parent( liam, don ).
       |parent( olivia, don ).
       |parent( liam, mia ).
       |parent( olivia, mia ).
       |parent( emma, rosie ).
       |parent( logan, rosie ).
       |parent( emma, aiden ).
       |parent( logan, aiden ).
       |
       |relation( X, Y ) :- ancestor( A, X ), ancestor( A, Y ), X \= Y.
       |ancestor( X, Y ) :- parent( X, Y ) ; parent( X, P ), ancestor( P, Y ).
       |
       |mother( X, Y ) :- female( X ), parent( X, Y ).
       |father( X, Y ) :- male( X ), parent( X, Y ).
       |daughter( X, Y ) :- female( X ), parent( Y, X ).
       |son( X, Y ) :- male( X ), parent( Y, X ).
       |siblings( X, Y ) :- parent( P, X ), parent( P, Y ), X \= Y.
       |full_siblings( A, B ) :-
       |  parent( F, A ), parent( F, B ),
       |  parent( M, A ), parent( M, B ),
       |  A \= B, F \= M.
       |sister( X, Y ) :- female( X ), siblings( Y, X ).
       |brother( X, Y ) :- male( X ), siblings( Y, X ).
       |uncle( U, N ) :- male( U ), siblings( U, P ), parent( P, N ).
       |aunt( A, N ) :- female( A ), siblings( A, P ), parent( P, N ).
       |grandparent( X, Y ) :- parent( X, P ), parent( P, Y ).
       |grandmother( X, Y ) :- female( X ), grandparent( X, Y ).
       |grandfather( X, Y ) :- male( X ), grandparent( X, Y ).
    """.stripMargin
  val query =
    """
       |grandmother( X, _ )
    """.stripMargin
  val prog = new Program

//  Compiler.debug = true

  System.gc
  System.gc

  val start = System.currentTimeMillis

  Parser.source( new StringReader(code) ) match {
    case Parser.Match( ast, _ ) =>
      //println( ast )
      println( (System.currentTimeMillis - start) )
      Compiler.compile( ast, prog )
      //prog.printProcedures

      Parser.query( new StringReader(query) ) match {
        case Parser.Match( ast, _ ) =>
          implicit val query = new Program
          implicit val vars = new Vars
          val block = query.block( "query" )
          val vm = new VM( prog ) {trace = false; debug = false/*; out = new PrintStream( "debug" )*/}

          Compiler.compileGoal( ast, prog )
          //block.print
          println( vm.runfirst( block ) map (_ filter {case (k, _) => !vars.evalSet(k)} map { case (k, v) => s"$k = ${display(v)}" } mkString "\n") mkString "\n\n" )
          //println( vm.runall( block ) map (_ filter {case (k, _) => !vars.evalSet(k)} map { case (k, v) => s"$k = ${display(v)}" } mkString "\n") mkString "\n\n" )
        case m: Parser.Mismatch => m.error
      }
    case m: Parser.Mismatch => m.error
  }

}
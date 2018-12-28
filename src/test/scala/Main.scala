package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  val code =
    """
       |p( a ).
       |p( b ).
    """.stripMargin
  val query =
    """
       |A = writeln(asdf), call( A ), writeln( done )
    """.stripMargin
  val prog = new Program

//  Compiler.debug = true

//  System.gc
//  System.gc
//
//  val start = System.currentTimeMillis

  Parser.source( new StringReader(code) ) match {
    case Parser.Match( ast, _ ) =>
      //println( ast )
      //println( (System.currentTimeMillis - start) )
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
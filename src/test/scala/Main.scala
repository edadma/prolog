package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  val code =
    """
       |:- import( "library/lists" ).
    """.stripMargin
  val query =
    """
       |findall( X, member( X, [1, 2, 3] ), L )
    """.stripMargin
  var prog = new Program

  prog.loadPredef
//  Compiler.debug = true

//  System.gc
//  System.gc
//
//  val start = System.currentTimeMillis

  Parser.source( new StringReader(code) ) match {
    case Parser.Match( ast, _ ) =>
      //println( ast )
      //println( (System.currentTimeMillis - start) )
      Compilation.compile( ast, prog )
      //prog.printProcedures

      //val pcc = new ByteArrayOutputStream

      //prog.save( pcc )
//      prog.save( "test.pcc" )
      //dump( pcc.toByteArray, 0, 3 )

      //sys.exit
//      prog =
//        new Program {
//          load( "test.pcc" )
//        }

      Parser.query( new StringReader(query) ) match {
        case Parser.Match( ast, _ ) =>
          implicit val query = new Program
          implicit val vars = new Vars
          val block = query.block( "query" )
          val vm = new VM( prog ) {trace = false; debug = false/*; out = new PrintStream( "debug" )*/}

          Compilation.compileGoal( ast, prog )
          //block.print
          println( vm.runfirst( block ) map (_ filter {case (k, _) => !vars.evalSet(k)} map { case (k, v) => s"$k = ${display(v)}" } mkString "\n") mkString "\n\n" )
          //println( vm.runall( block ) map (_ filter {case (k, _) => !vars.evalSet(k)} map { case (k, v) => s"$k = ${display(v)}" } mkString "\n") mkString "\n\n" )
        case m: Parser.Mismatch => m.error
      }
    case m: Parser.Mismatch => m.error
  }

}
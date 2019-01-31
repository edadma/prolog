package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  val code =
    """
      |//:- import( "library/lists" ).
      |
      |//main :-
      |// open( "test", write, S, [] ),
      |// set_output( S ),
      |// writeln( "this is a test" ),
      |// writeln( "this is another test" ),
      |// close( S ).
    """.stripMargin
//    """
//       |name_value(String, Name, Value) :-
//       |        sub_string(String, Before, _, After, "="), !,
//       |        sub_string(String, 0, Before, _, NameString),
//       |        atom_string(Name, NameString),
//       |        sub_string(String, _, After, 0, Value).
//    """.stripMargin
  val query =
    """
      |main
      |//name_value( "name=value", Name, Value )
      |//put_char( a ), nl
      |//sub_string( "abcd", B, 2, A, S )
      |//findall( X, X = asdf, L )
    """.stripMargin
  var prog = new Program

  prog.loadPredef
//  Compiler.debug = true

//  System.gc
//  System.gc
//
//  val start = System.currentTimeMillis

  OldParser.source( new StringReader(code) ) match {
    case OldParser.Match( ast, _ ) =>
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

      OldParser.query( new StringReader(query) ) match {
        case OldParser.Match( ast, _ ) =>
          implicit val query = new Program
          implicit val vars = new Vars
          val block = query.block( "query" )
          val vm = new VM( prog ) {trace = false; debug = false/*; out = new PrintStream( "debug" )*/}

          Compilation.compileGoal( ast, prog )
          //block.print
          //println( vm.runfirst( block ) map (_ filter {case (k, _) => !vars.evalSet(k)} map { case (k, v) => s"$k = ${display(v)}" } mkString "\n") mkString "\n\n" )
          println( vm.runall( block ) map (_ filter {case (k, _) => !vars.evalSet(k)} map { case (k, v) => s"$k = ${display(v)}" } mkString "\n") mkString "\n\n" )
        case m: OldParser.Mismatch => m.error
      }
    case m: OldParser.Mismatch => m.error
  }

}
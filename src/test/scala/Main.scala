package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.StringReader
import xyz.hyperreal.recursive_descent_parser.{Failure, Success}


object Main extends App {

  val code =
    """
       |
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
      |writeln( asdf )
    """.stripMargin
  var prog = new Program

  prog.loadPredef
//  Compiler.debug = true

//  System.gc
//  System.gc
//
//  val start = System.currentTimeMillis

  PrologParser.parseSource( new StringReader(code) ) match {
    case Success( ast, _ ) =>
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

      PrologParser.expression( PrologParser.lexer.tokenStream(new StringReader(query)) ) match {
        case Success( ast, _ ) =>
          implicit val query = new Program
          implicit val vars = new Vars
          val block = query.block( "query" )
          val vm = new VM( prog ) {trace = false; debug = false/*; out = new PrintStream( "debug" )*/}

          Compilation.compileGoal( ast, prog )
          //block.print
          //println( vm.runfirst( block ) map (_ filter {case (k, _) => !vars.evalSet(k)} map { case (k, v) => s"$k = ${display(v)}" } mkString "\n") mkString "\n\n" )
          println( vm.runall( block ) map (_ filter {case (k, _) => !vars.evalSet(k)} map { case (k, v) => s"$k = ${display(v)}" } mkString "\n") mkString "\n\n" )
        case f: Failure => f.error
      }
    case f: Failure => f.error
  }

}
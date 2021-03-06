package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.{Reader, StringReader}
import xyz.hyperreal.prolog.{Indicator, PrologParser, SinkStream, Structure, TextSourceStream, UserInput, VM, display, list2array}
import xyz.hyperreal.recursive_descent_parser.{Failure, Success}

import scala.collection.mutable


object TermIO {

  val repl: java.io.InputStream = null

  def write_term( vm: VM, pos: IndexedSeq[Reader], stream: Any, term: Any, options: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "write_term: stream is a variable" )
      case out: SinkStream =>
        term match {
          case _: vm.Variable => sys.error( "write_term: term is a variable" )
          case data =>
            val optionSet = mutable.HashSet[Symbol]()

            list2array( options ) match {
              case None => sys.error( "write_term: expected options list" )
              case Some( a ) =>
                for (o: Any <- a.toSet)
                  o match {
                    case Structure( Indicator(option@(Symbol("ignore_ops")|Symbol("numbervars")|Symbol("quoted")), 1), Array(set@(Symbol("true")|Symbol("false"))) ) =>
                      if (set == Symbol("true"))
                        optionSet += option
                    case option => sys.error( s"write_term: unrecognized option: $option" )
                  }
            }

            out.print( display(data, optionSet.toSet) )
            true
        }
    }

  def read_term( vm: VM, pos: IndexedSeq[Reader], stream: Any, term: Any, options: Any ) =
    stream match {
      case _: vm.Variable => sys.error( "read_term: stream is a variable" )
      case in: TextSourceStream =>
        val line = if (in == UserInput && repl != null) Console.withIn( repl ){ io.StdIn.readLine } else in.readLine

        PrologParser.expression( PrologParser.lexer.tokenStream(new StringReader(line)) ) match {
          case Success( ast, _ ) => vm.unify( vm.data(ast), term )
          case f: Failure => f.error
        }
    }

}
package xyz.hyperreal.prolog

import java.io.{File, PrintStream}

import jline.console.ConsoleReader
import jline.console.history.FileHistory

import xyz.hyperreal.pattern_matcher.{Reader, StringReader}


object Main extends App {

  REPL

  def REPL {
    val reader = new ConsoleReader
    val out = new PrintStream( reader.getTerminal.wrapOutIfNeeded(System.out), true )
    var line: String = null
    val historyFile = new File( System.getProperty("user.home") + "/.ppc-repl-history" )

    var program = new Program
    implicit var vars: Vars = null
    var block: Block = null
    var vm: VM = null

    if (!historyFile.exists)
      historyFile.createNewFile

    val history = new FileHistory( historyFile )

    sys.ShutdownHookThread {
      history.flush
    }

    reader.setBellEnabled( false )
    reader.setPrompt( "> " )
    reader.setHistory( history )
    out.println(
      """
        |Welcome to the PPC (Portable Prolog Compiler) REPL v0.1
        |PPC comes with ABSOLUTELY NO WARRANTY. This is free software.
        |Please type “;license” for legal details.
        |
        |Type “;help” for list of commands.
      """.trim.stripMargin )
    out.println

    while ({line = reader.readLine; line != null})
      if (line.trim nonEmpty) {
        try {
          if (line.headOption contains ';') {
            val command = line.drop( 1 ).trim split "\\s+" toList

            command match {
              case List("help" | "h") =>
                out.println(
                  """
                    |help (h)                         print this summary
                    |license                          print the license
                    |quit (q)                         exit the REPL
                  """.trim.stripMargin )
              case List("license") =>
                out.println(
                  """
                    |ISC License (ISC)
                    |
                    |Copyright 2018 Edward A. Maxedon, Sr.
                    |
                    |Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
                    |provided that the above copyright notice and this permission notice appear in all copies.
                    |
                    |THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL
                    |IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
                    |INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
                    |ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
                    |THIS SOFTWARE.
                  """.trim.stripMargin )
              case List( "quit"|"q" ) => sys.exit
              case List( "load"|"l", file ) =>
                program = new Program

                Parser.source( Reader.fromFile(file + ".prolog") ) match {
                  case Parser.Match( ast, _ ) =>
                    Compiler.compile( ast, program )

                    out.println( program.procedures map (_.func) mkString "\n" )
                  case m: Parser.Mismatch => m.error
                }
              case List( "" ) =>
                if (vm.fail)
                  vm.run( block ) match {
                    case Some( r ) => println( displayResult(r) )
                    case None => println( "no" )
                  }
                else
                  println( "no" )
            }
          } else {
            val all = line endsWith "*"
            val queryline = if (all) line dropRight 1 else line

            Parser.query( new StringReader(queryline) ) match {
              case Parser.Match( ast, _ ) =>
                implicit val query = new Program

                vars = new Vars
                block = query.block( "query" )
                vm = new VM( program )
                Compiler.compileGoal( ast, program )

                val result = if (all) vm.runall( block ) else vm.runfirst( block ).toList

                println( result map displayResult mkString "\n\n" )
              case m: Parser.Mismatch => m.error
            }
          }

          out.println
        } catch {
          case e: Exception =>
            out.println( e.getMessage )
            //e.printStackTrace( out )
            out.println
        }
      }

    def displayResult( r: Map[String, Any]) = r map { case (k, v) => s"$k = ${display(v)}" } mkString "\n"

  }
}
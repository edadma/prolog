package xyz.hyperreal.prolog

import java.nio.file.Paths

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.args.Options
import xyz.hyperreal.recursive_descent_parser.{Failure, Success}


object Compiler extends App {

  var predef = true
  var source: String = null
  var dest: String = null

  def usage( status: Int ) = {
    println(
      """
        |Prolog compiler v0.2
        |Usage:  java -cp <path/to/prolog-0.2.jar> xyz.hyperreal.prolog.Compiler <options> <source>
        |  where
        |    <source> is the path to the source file without .prolog extension
        |    <options> is one of
        |      --help      display this help and exit
        |      -d          destination directory
        |      -p          don't load predef
      """.trim.stripMargin )
    sys.exit( status )
  }

  Options( args ) {
    case "--help" :: _ => usage( 0 )
    case "-p" :: t =>
      predef = false
      t
    case "-d" :: path :: t =>
      dest = path
      t
    case o :: _ if o startsWith "-" =>
      println( "bad option: " + o )
      usage( 1 )
    case file :: t =>
      source = file
      t
  }

  if (source eq null) {
    println( "missing source file" )
    usage( 1 )
  }

  val path = Paths get source toAbsolutePath
  val dir =
    if (dest eq null)
      path.getParent
    else
      Paths get dest

  PrologParser.parseSource( Reader.fromFile(path.getParent resolve (s"${path.getFileName}.prolog") toString) ) match {
    case Success( ast, _ ) =>
      val prog = new Program

      if (predef)
        prog.loadPredef

      Compilation.compile( ast, prog )
      prog.save( dir resolve (s"${path.getFileName}.pcc") toString )
    case f: Failure => f.error
  }

}
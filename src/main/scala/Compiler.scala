package xyz.hyperreal.prolog

import java.nio.file.{Paths, Path}

import xyz.hyperreal.pattern_matcher.Reader

import xyz.hyperreal.args.Options


object Compiler extends App {

  var predef = true
  var source: String = null
  var dest: String = null

  def usage( status: Int ) = {
    """
      |Prolog compiler v0.1
      |Usage:  java -cp <path/to/prolog-0.1.jar> xyz.hyperreal.prolog.Compiler <options> <source>
      |  where
      |    <source> is the path to the source file without .prolog extension
      |    <options> is one of
      |      --help      display this help and exit
      |      -d          destination directory
      |      -p          don't load predef
    """.trim.stripMargin.lines foreach println
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

  val path = Paths get source
  val dir =
    if (dest eq null)
      path.getParent
    else
      Paths get dest

  Parser.source( Reader.fromFile(path.getParent resolve (path.getFileName + ".prolog") toString) ) match {
    case Parser.Match( ast, _ ) =>
      val prog = new Program

      if (predef)
        prog.loadPredef

      Compilation.compile( ast, prog )
      prog.save( dir resolve (path.getFileName + ".pcc") toString )
    case m: Parser.Mismatch => m.error
  }

}
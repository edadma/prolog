package xyz.hyperreal.prolog.builtin

import java.io.PrintStream

import xyz.hyperreal.prolog.VM


object CharacterIO {

  def put_char( vm: VM, s: Any, char: Any ) =
    (s, char) match {
      case (p: PrintStream, Symbol( c )) if c.length == 1 =>
        p print c.head
        true
      case _ => sys.error( "put_char: expected an output stream and a one character atom")
    }

}

package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.prolog.VM


object CharacterIO {

  def nl( vm: VM ) = {
    println
    true
  }

}

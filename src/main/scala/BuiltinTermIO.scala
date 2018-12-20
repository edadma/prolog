package xyz.hyperreal.prolog


object BuiltinTermIO {

  def write( vm: VM, a: Any ) = {
    print( display(a) )
    true
  }

  def writeln( vm: VM, a: Any ) = {
    println( display(a) )
    true
  }

}
package xyz.hyperreal.prolog


object BuiltinTermIO {

  def write( vm: VM, a: Any ) = {
    print( display(a) )
    true
  }

}
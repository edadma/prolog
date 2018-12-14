package xyz.hyperreal.prolog


object BuiltinTermIO {

  def write( a: Any ) = {
    print( display(a) )
    true
  }

}
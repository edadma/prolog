package xyz.hyperreal.prolog


object BuiltinControl {

  def halt( vm: VM ) = sys.exit

  def halt( vm: VM, status: Any ) = sys.exit( status.asInstanceOf[Int] )

}
package xyz.hyperreal.prolog


object Builtins {

  val predicates =
    Map[Functor, VM => Unit] (
      functor( "write", 2 ) -> write,
      functor( "nl", 0 ) -> nl
    )

  def write( vm: VM ): Unit = {
    print( vm.popValue )
  }

  def nl( vm: VM ): Unit = {
    println
  }

}
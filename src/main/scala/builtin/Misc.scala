package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{domainError, instantiationError, typeError, VM}


object Misc {

  def between( vm: VM, pos: IndexedSeq[Reader], lower: Any, upper: Any, value: Any ) =
    (lower, upper) match {
      case (_: vm.Variable, _) => instantiationError( pos(0), "lower must be given", 'between, 3 )
      case (_, _: vm.Variable) => instantiationError( pos(1), "upper must be given", 'between, 3 )
      case (l: Int, u: Int) if l <= u =>
        value match {
          case v: vm.Variable =>
            if (l == u)
              v bind l
            else {
              vm.resatisfyable(
                new (VM => Boolean) {
                  var next = l + 1

                  def apply( v1: VM ): Boolean = {
                    if (next == u)
                      v bind u
                    else {
                      vm.resatisfyable( this )

                      val cur = next

                      next += 1
                      v bind cur
                    }
                  }
                }
              )
              v bind l
            }
          case x: Int => l <= x || x <= u
          case _ => typeError( pos(2), "value must be a variable or an integer", 'integer, value, 'between, 3 )
        }
      case (l: Int, u: Int) => sys.error( "lower must be less than or equal to upper" )
    }

}
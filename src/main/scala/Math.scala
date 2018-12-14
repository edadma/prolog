package xyz.hyperreal.prolog

import java.lang.reflect.{Method, Modifier}

import scala.collection.mutable


object Math {

  private val functions = new mutable.HashMap[Functor, VM => Unit]
  private val NumberClass = classOf[Number]

  def exists( f: Functor ) = functions contains f

  def function( f: Functor ) = functions(f)

  def load( obj: Any ): Unit =
    for (m <- obj.getClass.getDeclaredMethods if m.getModifiers == Modifier.PUBLIC && !m.isSynthetic) {
      val name = m.getName

      m.getReturnType match {
        case NumberClass => functions(functor(name, m.getParameterCount)) = new Function(obj, m)
        case _ =>
      }
    }

  class Function( obj: Any, method: Method ) extends (VM => Unit) {
    def apply( vm: VM ) =
      vm.push( method.invoke( obj, (for (_ <- 1 to method.getParameterCount) yield vm.pop).reverse.
        toArray.asInstanceOf[Array[Object]]: _* ).asInstanceOf[Number] )

    override def toString(): String = s"<function ${method.getName}/${method.getParameterCount}>"
  }

  List(
    MathFunctions
  ) foreach load

}

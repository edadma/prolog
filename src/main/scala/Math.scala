package xyz.hyperreal.prolog

import java.lang.reflect.{Method, Modifier}

import xyz.hyperreal.pattern_matcher.Reader

import scala.collection.mutable


object Math {

  private val functions = new mutable.HashMap[Indicator, Function]
  private val NumberClass = classOf[Number]

  def exists( f: Indicator ) = functions contains f

  def function( f: Indicator ) = functions(f)

  def load( obj: Any ): Unit =
    for (m <- obj.getClass.getDeclaredMethods if m.getModifiers == Modifier.PUBLIC && !m.isSynthetic) {
      val name = m.getName

      m.getReturnType match {
        case NumberClass => functions(indicator(name, m.getParameterCount)) = new Function(obj, m)
        case _ =>
      }
    }

  class Function( obj: Any, method: Method ) extends ((VM, IndexedSeq[Reader]) => Unit) {
    def apply( vm: VM, pos: IndexedSeq[Reader] ) =
      vm.push( method.invoke( obj, (for (_ <- 1 to method.getParameterCount) yield vm.pop).reverse.
        toArray.asInstanceOf[Array[Object]]: _* ).asInstanceOf[Number] )

    def call( args: Array[Number] ) = method.invoke( obj,args.asInstanceOf[Array[Object]]: _* ).asInstanceOf[Number]

    override def toString(): String = s"<function ${method.getName}/${method.getParameterCount}>"
  }

  List(
    MathFunctions,
    MathConstants,
    MathSpecial
  ) foreach load

}
package xyz.hyperreal.prolog

import scala.collection.generic.Growable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class Program extends Growable[Instruction] {

  var code: ArrayBuffer[Instruction] = _
  val procedureMap = new mutable.HashMap[Functor, Procedure]

  def block( name: String ) = {
    val b = new Block( name )

    code = b.code
    b
  }

  def apply( n: Int ) = code(n)

  def procedures = procedureMap.values

  def pointer = code.length

  def patch( f: (Int, Int) => Instruction )( c: => Unit ): Unit = {
    val ptr = pointer

    code += null
    c
    code(ptr) = f( ptr, code.length )
  }

  def +=( inst: Instruction ) = {
    code += inst
    this
  }

  def clear = {
    code.clear
    procedureMap.clear
  }

  def printProcedures: Unit =
    for (Procedure( Functor(Symbol(name), arity), block, start, end, clauses ) <- procedureMap.values.toList.sorted) {
      println( s"$name/$arity" )

      if (clauses isEmpty)
        println( "  undefined\n" )
      else
        block.print( start, end )

      println
    }

  def defined( name: String, arity: Int ): Boolean = defined( functor(name, arity) )

  def defined( f: Functor ) = procedureMap contains f

  def get( f: Functor ) = procedureMap get f

  def procedure( name: String, arity: Int ): Procedure = procedure( functor(name, arity) )

  def procedure( f: Functor ) =
    procedureMap get f match {
      case None =>
        val p = Procedure( f, null, -1, 0 )

        procedureMap(f) = p
        p
      case Some( p ) => p
    }

}

class Block( val name: String ) {

  val code: ArrayBuffer[Instruction] = new ArrayBuffer

  def apply( idx: Int ) = code(idx)

  def length = code.length

  def print( start: Int, end: Int ) =
    for (i <- start until end) {
      println( "  " + instruction(code(i)) )
    }

  def print: Unit = print( 0, code.length )

  override def toString: String = s"[block $name]"

}
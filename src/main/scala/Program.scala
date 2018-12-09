package xyz.hyperreal.prolog

import scala.collection.generic.Growable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class Program extends Growable[Instruction] {

  val code = new ArrayBuffer[Instruction]
  val procedureMap = new mutable.HashMap[Functor, Procedure]

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

//  def print: Unit = {
//    for (Procedure( Functor(Symbol(name), arity), clauses ) <- procedures.values.toList.sorted) {
//      println( s"$name/$arity" )
//
//      if (clauses isEmpty)
//        println( "  undefined\n" )
//      else
//        for (c <- clauses) {
//          for (ins <- c.code)
//            println( "  " + (ins match {
//              case EnterInstruction => "enter"
//              case ExitInstruction => "exit"
//              case PopInstruction => "pop"
//              case VarInstruction( n ) => s"var $n"
//              case CallInstruction( Procedure(Functor(Symbol(name), arity), _) ) => s"call $name/$arity"
//              case ConstInstruction( AtomData(Symbol(name)) ) => s"const '$name'"
//              case ConstInstruction( IntegerData(n) ) => s"const $n"
//              case EvalInstruction( v, v1 ) => s"eval $v, $v1"
//              case PushVarInstruction( n ) => s"pushv $n"
//              case PushNumInstruction( n ) => s"push $n"
//              case AddInstruction => "add"
//              case ResultInstruction( n ) => s"result $n"
//            }) )
//
//          println
//        }
//    }
//  }

  def defined( name: String, arity: Int ) = procedureMap contains functor( name, arity )

//  def get( name: String, arity: Int ) = get( functor(name, arity) )
//
//  def get( f: Functor ) = procedureMap get f

  def procedure( name: String, arity: Int ) = {
    val f = functor( name, arity )

    procedureMap get f match {
      case None =>
        val p = Procedure( f, 0 )

        procedureMap(f) = p
        p
      case Some( p ) => p
    }
  }

}
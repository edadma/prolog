package xyz.hyperreal.prolog

import scala.collection.mutable


class Program {

  val procedures = new mutable.HashMap[Functor, Procedure]

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

  def procedure( name: String, arity: Int ) = {
    val f = Functor( Symbol(name), arity )

    procedures get f match {
      case None =>
        val p = Procedure( f )

        procedures(f) = p
        p
      case Some( p ) => p
    }
  }

  def clause( name: String, arity: Int, vars: Int, entry: Int ): Unit =
    procedure( name, arity ).clauses += Clause( vars, entry )

}
package xyz.hyperreal.prolog

import scala.collection.generic.Growable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class Program extends Growable[Instruction] {

  val code = new ArrayBuffer[Instruction]
  val procedureMap = new mutable.HashMap[Functor, Procedure]
  var fixups = new ArrayBuffer[(Int, Functor)]

  def apply( n: Int ) = code(n)

  def update( n: Int, inst: Instruction ) = code(n) = inst

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

  def print: Unit = {
    for (Procedure( Functor(Symbol(name), arity), entry, end, clauses ) <- procedureMap.values.toList.sorted) {
      println( s"$name/$arity" )

      if (clauses isEmpty)
        println( "  undefined\n" )
      else
        for (i <- entry until end) {
          println( "  " + (code(i) match {
            case PushAtomicInst( d ) => s"push $d"
            case PushVarInst( n ) => s"pushv $n"
            case PushCompoundInst( Functor(Symbol(name), arity) ) => s"pushf $name/$arity"
            case PushElementInst( n ) => s"pushe $n"
            case ReturnInst => s"return"
            case VarBindInst( n ) => s"bind $n"
            case FunctorInst( Functor(Symbol(name), arity) ) => s"functor $name/$arity"
            case DupInst => "dup"
            case EqInst => "eq"
            case BranchIfInst( disp ) => s"branchif $disp"
            case FailInst => "fail"
            case ChoiceInst( disp ) => s"choice $disp"
            case CallInst( entry ) => s"call $entry"
            case DropInst => "drop"
            case PushFrameInst => "pushfr"
            case FrameInst( vars ) => s"frame $vars"
            case PredicateInst( pred ) => s"pred $pred"
            case UnifyInst => "unify"
          }) )
        }

      println
    }
  }

  def defined( name: String, arity: Int ) = procedureMap contains functor( name, arity )

  def get( f: Functor ) = procedureMap get f

  def fixup( f: Functor ) {
    fixups += ((pointer, f))
    code += null
  }

  def procedure( name: String, arity: Int ): Procedure = procedure( functor(name, arity) )

  def procedure( f: Functor ) =
    procedureMap get f match {
      case None =>
        val p = Procedure( f, -1, 0 )

        procedureMap(f) = p
        p
      case Some( p ) => p
    }

}
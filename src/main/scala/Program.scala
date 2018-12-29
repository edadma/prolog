package xyz.hyperreal.prolog

import java.io.{DataInputStream, DataOutputStream, InputStream, OutputStream}

import scala.collection.generic.Growable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class Program extends Growable[Instruction] {

  val ATOM = 0
  val INTEGER = 1
  val FLOAT = 2
  val STRING = 3
  val STRUCTURE = 4
  val VARIABLE = 5

  var code: ArrayBuffer[Instruction] = _
  val procedureMap = new mutable.HashMap[Functor, Procedure]

  def save( out: OutputStream ): Unit = {
    val s = new DataOutputStream( out ) { def ptr = written }

    def writeFunctor( f: Functor ): Unit = {
      s writeUTF f.name.name
      s writeByte f.arity
    }

    def write( d: Any ): Unit =
      d match {
        case Symbol( atom ) =>
          s writeByte ATOM
          s writeUTF atom
        case n: Int =>
          s writeByte INTEGER
          s writeInt n
        case n: Double =>
          s writeByte FLOAT
          s writeDouble n
        case a: String =>
          s writeByte STRING
          s writeUTF a
        case Structure( f, args ) =>
          s writeByte STRUCTURE
          writeFunctor( f )
          args foreach write
      }

    s writeBytes "PCC V1 "

    for (Procedure( func, block, _, _, _ ) <- procedureMap.values) {
      writeFunctor( func )

      val len = s.ptr

      s writeInt 0

      block.code foreach {
        case DebugInst( msg, _ ) =>
          s writeByte 0
          s writeUTF msg
        case PushInst( d ) =>
          s writeByte 1
          write( d )
        case VarInst( n ) =>
          s writeByte 2
          s writeByte n
        case VarUnifyInst( n ) =>
          s writeByte 3
          s writeByte n
        case StructureInst( f ) =>
          s writeByte 4
          writeFunctor( f )
        case ElementUnifyInst( n ) =>
          s writeByte 5
          s writeByte n
        case ReturnInst => s writeByte 6
        case FunctorInst( f ) =>
          s writeByte 7
          writeFunctor( f )
        case DupInst => s writeByte 8
        case EqInst => s writeByte 9
        case NeInst => s writeByte 10
        case LtInst => s writeByte 11
        case LeInst => s writeByte 12
        case GtInst => s writeByte 13
        case GeInst => s writeByte 14
        case BranchIfInst( disp ) =>
          s writeByte 15
          s writeInt disp
        case BranchInst( disp ) =>
          s writeByte 16
          s writeInt disp
        case FailInst => s writeByte 17
        case ChoiceInst( disp ) =>
          s writeByte 17
          s writeInt disp
        case CutChoiceInst( disp ) =>
          s writeByte 18
          s writeInt disp
        case CutInst => s writeByte 19
        case MarkInst( disp ) =>
          s writeByte 20
          s writeInt disp
        case UnmarkInst => s writeByte 21
        case CallBlockInst => s writeByte 22
        case CallProcedureInst( p ) =>
          s writeByte 23
          writeFunctor( p.func )
        case CallIndirectInst( _, f ) =>
          s writeByte 24
          writeFunctor( f )
        case DropInst => s writeByte 25
        case PushFrameInst => s writeByte 26
        case FrameInst( vars ) =>
          s writeByte 27
          s writeInt vars
        case NativeInst( _, func ) => s"native $pred"
        case UnifyInst => "unify"
        case EvalInst( _, _, v ) => s"eval $v"
        case AddInst => "add"
        case SubInst => "sub"
        case MulInst => "mul"
        case DivInst => "div"
      }
    }

  }

  def load( in: InputStream ): Unit = {
    val s = new DataInputStream( in )


  }

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
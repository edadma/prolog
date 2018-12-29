package xyz.hyperreal.prolog

import java.io._

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
    val s = new DataOutputStream( out )

    s writeBytes "PCC V1 "

    for (Procedure( Functor(Symbol(name), arity), block, _, _, _ ) <- procedureMap.values) {
      s writeUTF name
      s writeByte arity

      val blockpcc = new ByteArrayOutputStream
      val b = new DataOutputStream( blockpcc )

      def writeFunctor( f: Functor ): Unit = {
        b writeUTF f.name.name
        b writeByte f.arity
      }

      def write( d: Any ): Unit =
        d match {
          case Symbol( atom ) =>
            b writeByte ATOM
            b writeUTF atom
          case n: Int =>
            b writeByte INTEGER
            b writeInt n
          case n: Double =>
            b writeByte FLOAT
            b writeDouble n
          case a: String =>
            b writeByte STRING
            b writeUTF a
          case Structure( f, args ) =>
            b writeByte STRUCTURE
            writeFunctor( f )
            args foreach write
        }

      block.code foreach {
        case DebugInst( msg, _ ) =>
          b writeByte 0
          b writeUTF msg
        case PushInst( d ) =>
          b writeByte 1
          write( d )
        case VarInst( n ) =>
          b writeByte 2
          b writeByte n
        case VarUnifyInst( n ) =>
          b writeByte 3
          b writeByte n
        case StructureInst( f ) =>
          b writeByte 4
          writeFunctor( f )
        case ElementUnifyInst( n ) =>
          b writeByte 5
          b writeByte n
        case ReturnInst => b writeByte 6
        case FunctorInst( f ) =>
          b writeByte 7
          writeFunctor( f )
        case DupInst => b writeByte 8
        case EqInst => b writeByte 9
        case NeInst => b writeByte 10
        case LtInst => b writeByte 11
        case LeInst => b writeByte 12
        case GtInst => b writeByte 13
        case GeInst => b writeByte 14
        case BranchIfInst( disp ) =>
          b writeByte 15
          b writeInt disp
        case BranchInst( disp ) =>
          b writeByte 16
          b writeInt disp
        case FailInst => b writeByte 17
        case ChoiceInst( disp ) =>
          b writeByte 17
          b writeInt disp
        case CutChoiceInst( disp ) =>
          b writeByte 18
          b writeInt disp
        case CutInst => b writeByte 19
        case MarkInst( disp ) =>
          b writeByte 20
          b writeInt disp
        case UnmarkInst => b writeByte 21
        case CallBlockInst => b writeByte 22
        case CallProcedureInst( p ) =>
          b writeByte 23
          writeFunctor( p.func )
        case CallIndirectInst( _, f ) =>
          b writeByte 24
          writeFunctor( f )
        case DropInst => b writeByte 25
        case PushFrameInst => b writeByte 26
        case FrameInst( vars ) =>
          b writeByte 27
          b writeInt vars
        case NativeInst( _, func ) =>
          b writeByte 28
          writeFunctor( func )
        case UnifyInst => b writeByte 29
        case EvalInst( _, _, v ) =>
          b writeByte 30
          b writeInt v
        case AddInst => b writeByte 31
        case SubInst => b writeByte 32
        case MulInst => b writeByte 33
        case DivInst => b writeByte 34
      }

      s writeInt blockpcc.size
      s write blockpcc.toByteArray
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
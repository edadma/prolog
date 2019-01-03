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
  val loadSet = new mutable.HashSet[String]

  def save( s: String ): Unit = save( new FileOutputStream(s) )

  def save( out: OutputStream ): Unit = {
    val s = new DataOutputStream( out )

    s writeBytes "PCC V1 "
    s writeInt procedureMap.size

    for (Procedure( func, block, _, _, _ ) <- procedureMap.values) {
      def writeFunctor( f: Functor ): Unit = {
        s writeUTF f.name.name
        s writeByte f.arity
      }

      writeFunctor( func )
      //      s writeUTF name
//      s writeByte arity
      s writeInt block.length

//      val blockpcc = new ByteArrayOutputStream
//      val b = new DataOutputStream( blockpcc )

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

      block.code foreach {
        case DebugInst( msg, _ ) =>
          s writeByte 0
          s writeUTF msg
        case PushInst( d ) =>
          s writeByte 1
          write( d )
        case PushVarInst( n ) =>
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
          s writeByte 18
          s writeInt disp
        case CutChoiceInst( disp ) =>
          s writeByte 19
          s writeInt disp
        case CutInst => s writeByte 20
        case MarkInst( disp ) =>
          s writeByte 21
          s writeInt disp
        case UnmarkInst => s writeByte 22
        case CallBlockInst => s writeByte 23
        case CallProcedureInst( p ) =>
          s writeByte 24
          writeFunctor( p.func )
        case CallIndirectInst( _, f ) =>
          s writeByte 25
          writeFunctor( f )
        case DropInst => s writeByte 26
        case PushFrameInst => s writeByte 27
        case FrameInst( vars ) =>
          s writeByte 28
          s writeByte vars
        case NativeInst( _, func, group ) =>
          s writeByte 29
          s writeByte group
          writeFunctor( func )
        case UnifyInst => s writeByte 30
        case EvalInst( _, name, v ) =>
          s writeByte 31
          s writeUTF name
          s writeByte v
        case AddInst => s writeByte 32
        case SubInst => s writeByte 33
        case MulInst => s writeByte 34
        case DivInst => s writeByte 35
        case TermEqInst => s writeByte 36
        case TermLtInst => s writeByte 37
        case TermLeInst => s writeByte 38
        case VarInst => s writeByte 39
        case NonvarInst => s writeByte 40
        case NilUnifyInst => s writeByte 41
      }

//      s writeInt blockpcc.size
//      s write blockpcc.toByteArray
    }

    out.close
  }

  def loadPredef = loadResource( "$predef" )

  def loadResource( name: String ) = load( new DataInputStream(getClass.getResourceAsStream(name + ".pcc")), null )

  def load( s: String ): List[Functor] =
    if (loadSet(s))
      Nil
    else {
      val f = new RandomAccessFile( s, "r" )

      if (f.length < 7 + 4)
        sys.error( "load: invalid pcc file: too short" )

      val loaded = new ArrayBuffer[Functor]

      load( f, loaded )
      loaded.sorted.toList
    }

  def load( s: DataInput, loaded: ArrayBuffer[Functor] ): Unit = {
    val magic = new Array[Byte]( 7 )

    s readFully magic

    if (new String(magic) != "PCC V1 ")
      sys.error( "load: invalid pcc file: wrong magic value" )

    val procs = s.readInt

    if (procs <= 0)
      sys.error( s"load: invalid pcc file: expected positive number of procedures" )

    def readFunctor = functor( s.readUTF, s.readByte )

    for (_ <- 1 to procs) {
      val p = procedure( readFunctor )

      if (loaded ne null)
        loaded += p.func

      p.block = block( p.func.toString )
      p.entry = pointer

      def read: Any =
        s readUnsignedByte match {
          case ATOM => Symbol( s readUTF )
          case INTEGER => s.readInt
          case FLOAT => s.readDouble
          case STRING => s.readUTF
          case STRUCTURE =>
            val f = readFunctor

            Structure( f, (for (_ <- 1 to f.arity) yield read).toArray )
        }

      for (_ <- 1 to s.readInt)
        code +=
          (s readUnsignedByte match {
            case 0 => DebugInst( s.readUTF, null )
            case 1 => PushInst( read )
            case 2 => PushVarInst( s.readUnsignedByte )
            case 3 => VarUnifyInst( s.readUnsignedByte )
            case 4 => StructureInst( readFunctor )
            case 5 => ElementUnifyInst( s.readUnsignedByte )
            case 6 => ReturnInst
            case 7 => FunctorInst( readFunctor )
            case 8 => DupInst
            case 9 => EqInst
            case 10 => NeInst
            case 11 => LtInst
            case 12 => LeInst
            case 13 => GtInst
            case 14 => GeInst
            case 15 => BranchIfInst( s.readInt )
            case 16 => BranchInst( s.readInt )
            case 17 => FailInst
            case 18 => ChoiceInst( s.readInt )
            case 19 => CutChoiceInst( s.readInt )
            case 20 => CutInst
            case 21 => MarkInst( s.readInt )
            case 22 => UnmarkInst
            case 23 => CallBlockInst
            case 24 => CallProcedureInst( procedure(readFunctor) )
            case 25 => CallIndirectInst( null, readFunctor )
            case 26 => DropInst
            case 27 => PushFrameInst
            case 28 => FrameInst( s.readUnsignedByte )
            case 29 =>
              val group = s.readUnsignedByte
              val func = readFunctor
              val native =
                group match {
                  case NATIVE_PREDICATE => Builtin predicate func
                  case NATIVE_MATH => Math function func
                  case NATIVE_RUNTIME =>
                    func.name match {
                      case '$compile => Runtime.compileCall _
                    }
                }

              NativeInst( native, func, group )
            case 30 => UnifyInst
            case 31 => EvalInst( null, s.readUTF, s.readUnsignedByte )
            case 32 => AddInst
            case 33 => SubInst
            case 34 => MulInst
            case 35 => DivInst
            case 36 => TermEqInst
            case 37 => TermLtInst
            case 38 => TermLeInst
            case 39 => VarInst
            case 40 => NonvarInst
            case 41 => NilUnifyInst
          })
    }
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
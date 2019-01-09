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
  val ANONYMOUS = 6

  var code: ArrayBuffer[Instruction] = _
  val procedureMap = new mutable.HashMap[Functor, Procedure]
  val blockMap = new mutable.HashMap[String, Block]
  val loadSet = new mutable.HashSet[String]

  def save( s: String ): Unit = save( new FileOutputStream(s) )

  def save( out: OutputStream ): Unit = {
    val s = new DataOutputStream( out )

    s writeBytes "PCC V1 "
    s writeInt procedureMap.size

    for (Procedure( func, pblock, pub, clauses ) <- procedureMap.values) {
      def writeFunctor( f: Functor ): Unit = {
        s writeUTF f.name.name
        s writeByte f.arity
      }

      s writeBoolean pub
      writeFunctor( func )

      if (pub) {
        s writeInt clauses.length

        for (Clause( ast, cblock ) <- clauses) {
          writeTerm( ast )
          writeBlock( cblock )
        }
      } else
        writeBlock( pblock )

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

      def writeTerm( c: TermAST ): Unit =
        c match {
          case AtomAST( _, atom ) =>
            s writeByte ATOM
            s writeUTF atom
          case IntegerAST( _, n ) =>
            s writeByte INTEGER
            s writeInt n
          case FloatAST( _, n ) =>
            s writeByte FLOAT
            s writeDouble n
          case StringAST( _, a ) =>
            s writeByte STRING
            s writeUTF a
          case StructureAST( _, name, args ) =>
            s writeByte STRUCTURE
            s writeUTF name
            s writeByte args.length
            args foreach writeTerm
          case AnonymousAST( _ ) => s writeByte ANONYMOUS
          case VariableAST( _, v ) =>
            s writeByte VARIABLE
            s writeUTF v
        }

      def writeBlock( b: Block ) = {
        s writeInt b.length

        b.code foreach {
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
          case NopInst => s writeByte 42
          case JumpInst( b ) =>
            s writeByte 43
            s writeUTF b.name
        }
      }
    }

    out.close
  }

  def loadPredef = loadAsResource( "$predef" )

  def loadAsResource( name: String ) = {
    val res = getClass.getResourceAsStream( name + ".pcc" )

    if (res eq null)
      sys.error( s"code resource not found: $name" )

    val loaded = new ArrayBuffer[Functor]

    load( new DataInputStream(res), loaded )
    loaded.sorted.toList
  }

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

  def load( s: DataInput, loaded: ArrayBuffer[Functor] ) = {
    val magic = new Array[Byte]( 7 )

    s readFully magic

    if (new String(magic) != "PCC V1 ")
      sys.error( "load: invalid pcc file: wrong magic value" )

    val procs = s.readInt

    if (procs <= 0)
      sys.error( s"load: invalid pcc file: expected positive number of procedures" )

    def readFunctor = functor( s.readUTF, s.readByte )

    for (_ <- 1 to procs) {
      val pub = s readBoolean
      val p = procedure( readFunctor, pub )

      if (loaded ne null)
        loaded += p.func

      if (pub) {
        for (i <- 1 to s.readInt) {
          p.clauses += Clause( readTerm, block(s"${p.func} $i") )
          readBlock
        }
      } else {
        p.block = block( p.func.toString )
        readBlock
      }

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

      def readTerm: TermAST =
        s readUnsignedByte match {
          case ATOM => AtomAST( null, s readUTF )
          case INTEGER => IntegerAST( null, s.readInt )
          case FLOAT => FloatAST( null, s.readDouble )
          case STRING => StringAST( null, s.readUTF )
          case STRUCTURE =>
            val name = s.readUTF
            val argc = s.readByte

            StructureAST( null, name, (for (_ <- 1 to argc) yield readTerm).toList )
          case VARIABLE => VariableAST( null, s.readUTF )
          case ANONYMOUS => AnonymousAST( null )
        }

      def readBlock =
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
              case 42 => NopInst
              case 43 => JumpInst( block(s.readUTF) )
            })
    }
  }

  def block( name: String ) = {
    val b =
      blockMap get name match {
        case None =>
          val b = new Block( name )

          blockMap(name) = b
          b
        case Some( bl ) => bl
      }

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
    for (Procedure( Functor(Symbol(name), arity), block, pub, clauses ) <- procedureMap.values.toList.sorted) {
      println( s"$name/$arity" )

      if (block eq null)
        clauses foreach {
          case Clause( ast, cblock ) =>
            println( ast )
            cblock.print
        }
      else
        block.print

      println
    }

  def defined( name: String, arity: Int ): Boolean = defined( functor(name, arity) )

  def defined( f: Functor ) = procedureMap contains f

  def get( f: Functor ) = procedureMap get f

  def clause( f: Functor, ast: TermAST ): Unit = {
    val p = procedure( f )

    p.clauses += Clause( ast, block(s"$f ${p.clauses.length + 1}") )
  }

  def procedure( name: String, arity: Int, pub: Boolean ): Procedure = procedure( functor(name, arity), pub )

  def procedure( f: Functor, pub: Boolean = true ) =
    procedureMap get f match {
      case None =>
        val p = Procedure( f, null, pub )

        procedureMap(f) = p
        p
      case Some( p ) => p
    }

}

class Block( val name: String ) {

  val code: ArrayBuffer[Instruction] = new ArrayBuffer

  def apply( idx: Int ) = code(idx)

  def update( n: Int, elem: Instruction ) = code(n) = elem

  def length = code.length

  def print( start: Int, end: Int ) =
    for (i <- start until end) {
      println( "  " + instruction(code(i)) )
    }

  def print: Unit = print( 0, code.length )

  override def toString: String = s"[block $name]"

}
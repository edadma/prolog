package xyz.hyperreal

import java.io.PrintStream

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}


package object prolog {

  implicit val symbolOrdering = Ordering by Symbol.unapply
  implicit val functorOrdering = Ordering by Functor.unapply
  implicit val procedureOrdering = Ordering by [Procedure, Functor] (_.func)

  val NIL = Symbol( "[]" )
  val CONS = Functor( Symbol("."), 2 )

  val NATIVE_PREDICATE = 0
  val NATIVE_MATH = 1
  val NATIVE_RUNTIME = 2

  case object EMPTY { override def toString = "[empty]" }

  case class Functor( name: Symbol, arity: Int ) { override def toString = s"${name.name}/$arity" }

  case class Procedure( func: Functor, var block: Block, var entry: Int, var end: Int, clauses: ListBuffer[Clause] = new ListBuffer ) { override def toString = s"[procedure $func]" }

  case class Clause( var vars: Int, ast: TermAST )

  trait Compound extends Product {
    def update( n: Int, value: Any )

    override def productElement( n: Int ): Any
  }

  case class Structure( functor: Functor, args: Array[Any] ) extends Compound {
    override def productArity = args.length

    override def productElement( n: Int ): Any = args( n )

    override def productPrefix = functor.name.name

    def update( n: Int, v: Any ) = args(n) = v
  }

  class Vars {
    val varMap = new mutable.HashMap[String, Int]
    val evals = new mutable.HashSet[String]

    def count = varMap.size

    def anon = num( '$' + count.toString )

    def num( name: String ) = {
      varMap get name match {
        case None =>
          val n = count

          varMap(name) = n
          n
        case Some( n ) => n
      }
    }

    def get( name: String ) = varMap get name

    def eval( name: String ) =
      if (evals( name ))
        false
      else {
        evals += name
        true
      }

    def evalSet = evals map (_ + '\'') toSet
  }

  def groundTerm( term: Any ): Boolean =
    term match {
      case Structure( _, args ) => args forall groundTerm
      case _: Symbol | _: Number => true
      case _: VM#Variable => false
    }

  def vareval( a: Any ): Any =
    a match {
      case v: VM#Variable => v.eval
      case _ => a
    }

  def list2array( s: Any, buf: ArrayBuffer[Any] = new ArrayBuffer ): Option[Array[Any]] =
    s match {
      case NIL => Some( buf.toArray )
      case Structure( CONS, Array(head, tail) ) =>
        buf += head
        list2array( tail, buf )
      case _ => None
    }

  def array2list( a: Array[Any] ) = {
    var list: Any = NIL
    var idx = a.length - 1

    while (idx >= 0) {
      list = cons( a(idx), list )
      idx -= 1
    }

    list
  }

  def cons( head: Any, tail: Any ) = Structure( CONS, Array(head, tail) )

  def functor( name: String, arity: Int ) = Functor( Symbol(name), arity )

  def display( a: Any ): String =
    vareval( a ) match {
      case Symbol( s ) => s
      case Structure( CONS, Array(_, _) ) =>
        def elems( term: Any, buf: StringBuilder = new StringBuilder ): String =
          term match {
            case NIL => buf.toString
            case Structure( CONS, Array(hd, tl) ) =>
              buf ++= display( hd )

              val tl1 = vareval( tl )

              tl1 match {
                case NIL =>
                case s: Structure if s.functor == CONS => buf ++= ", "
                case _ => buf ++= " | "
              }

              elems( tl1, buf )
            case e =>
              buf ++= display( e )
              buf.toString
          }

        s"[${elems( a )}]"
      case Structure( Functor(Symbol(name), _), args ) => s"$name(${args.map(display).mkString(",")})"
      case v => v.toString
    }

  def instruction( inst: Instruction ) =
    inst match {
      case null => "*** null ***"
      case DebugInst( msg, null ) => s"-----  $msg"
      case DebugInst( msg, pos ) => s"-----  $msg -- ${pos.line}:${pos.col}"
      case PushInst( d ) => s"push $d"
      case VarInst( n ) => s"pushv $n"
      case VarUnifyInst( n ) => s"unifyv $n"
      case StructureInst( Functor(Symbol(name), arity) ) => s"pushf $name/$arity"
      case ElementUnifyInst( n ) => s"unifye $n"
      case ReturnInst => s"return"
      case FunctorInst( Functor(Symbol(name), arity) ) => s"functor $name/$arity"
      case DupInst => "dup"
      case EqInst => "eq"
      case NeInst => "ne"
      case LtInst => "lt"
      case LeInst => "le"
      case GtInst => "gt"
      case GeInst => "ge"
      case BranchIfInst( disp ) => s"branch if $disp"
      case BranchInst( disp ) => s"branch $disp"
      case FailInst => "fail"
      case ChoiceInst( disp ) => s"choice $disp"
      case CutChoiceInst( disp ) => s"cut_choice $disp"
      case CutInst => "cut"
      case MarkInst( disp ) => s"mark $disp"
      case UnmarkInst => "unmark"
      case CallBlockInst => "call block"
      case CallProcedureInst( p ) => s"call $p"
      case CallIndirectInst( _, f ) => s"call $f"
      case DropInst => "drop"
      case PushFrameInst => "pushfr"
      case FrameInst( vars ) => s"frame $vars"
      case NativeInst( pred, _, _ ) => s"native $pred"
      case UnifyInst => "unify"
      case EvalInst( _, _, v ) => s"eval $v"
      case AddInst => "add"
      case SubInst => "sub"
      case MulInst => "mul"
      case DivInst => "div"
    }

  def dump( array: Array[Byte], start: Int, lines: Int, out: PrintStream = Console.out ) = {
    val addr = start - start%16

    def printByte( b: Option[Int] ) =
      if (b isEmpty)
        out.print( "-- " )
      else
        out.print( "%02x ".format(b.get&0xFF).toUpperCase )

    def printChar( c: Option[Int] ) = out.print( if (c.nonEmpty && ' ' <= c.get && c.get <= '~') c.get.asInstanceOf[Char] else '.' )

    def read( addr: Int ) =
      if (addr < array.length)
        Some( array(addr)&0xFF )
      else
        None

    for (line <- addr until (addr + 16*lines) by 16) {
      out.print( "%6x  ".format(line).toUpperCase )

      for (i <- line until (line + 16)) {
        if (i%16 == 8)
          out.print( ' ' )

        printByte( read(i) )
      }

      val bytes = ((line + 16) min 0x10000) - line

      out.print( " "*((16 - bytes)*3 + 1 + (if (bytes < 9) 1 else 0)) )

      for (i <- line until (line + 16))
        printChar( read(i) )

      out.println
    }
  }

}

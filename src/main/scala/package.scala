package xyz.hyperreal

import scala.collection.mutable.ListBuffer


package object prolog {

  implicit val symbolOrdering = Ordering by Symbol.unapply
  implicit val functorOrdering = Ordering by Functor.unapply
  implicit val procedureOrdering = Ordering by [Procedure, Functor] (_.func)

  val NIL = Symbol( "[]" )
  val CONS = Functor( Symbol("."), 2 )

  case object WILDCARD {
    override def toString = "_"
  }

  object EMPTY

  case class Functor( name: Symbol, arity: Int )

  case class Procedure( func: Functor, var entry: Int, var end: Int, clauses: ListBuffer[Clause] = new ListBuffer )

  case class Clause( var vars: Int, ast: TermAST )

  trait Compound extends Product {
    def update( n: Int, value: Any )
  }

  case class Structure( functor: Functor, args: Array[Any] ) extends Compound {
    override def productArity = args.length

    override def productElement( n: Int ) = args( n )

    def update( n: Int, v: Any ) = args(n) = v
  }

  def functor( name: String, arity: Int ) = Functor( Symbol(name), arity )

  def display( a: Any ): String =
    a match {
      case Symbol( s ) => s
      case Structure( CONS, Array(_, _) ) =>
        def elems( term: Any, buf: StringBuilder = new StringBuilder ): String =
          term match {
            case NIL => buf.toString
            case Structure( CONS, Array(hd, tl) ) =>
              buf ++= display( hd )

              tl match {
                case NIL =>
                case s: Structure if s.functor == CONS => buf ++= ", "
                case _ => buf ++= " | "
              }

              elems( tl, buf )
            case e =>
              buf ++= display( e )
              buf.toString
          }

        s"[${elems( a )}]"
      case Structure( Functor(Symbol(name), _), args ) => s"$name(${args.map(display).mkString(",")})"
      case v: VM#Variable =>
        val x = v.eval

        if (x.isInstanceOf[VM#Variable])
          x.toString
        else
          display( x )
      case _ => a.toString
    }

}

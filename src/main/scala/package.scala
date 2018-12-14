package xyz.hyperreal

import scala.collection.mutable.ListBuffer


package object prolog {

  implicit val symbolOrdering = Ordering by Symbol.unapply
  implicit val functorOrdering = Ordering by Functor.unapply
  implicit val procedureOrdering = Ordering by [Procedure, Functor] (_.func)

  val NIL = Symbol( "[]" )
  val CONS = Functor( Symbol("."), 2 )

  case class Functor( name: Symbol, arity: Int )

  case class Procedure( func: Functor, var entry: Int, var end: Int, clauses: ListBuffer[Clause] = new ListBuffer )

  case class Clause( var vars: Int, ast: TermAST )

  case class Structure( functor: Functor, args: Vector[Any] ) extends Product {
    override def productArity = args.length

    override def productElement( n: Int ) = args( n )
  }

  def functor( name: String, arity: Int ) = Functor( Symbol(name), arity )

  case object Wildcard

  def display( a: Any ): String =
    a match {
      case Symbol( s ) => s
      case Structure( Functor(Symbol(name), _), args ) => s"$name(${args.map(display).mkString(",")})"
      case Structure( CONS, Vector(_, _) ) =>
        def elems( term: Any, buf: StringBuilder = new StringBuilder ): String =
          term match {
            case Structure( CONS, Vector(hd, tl) ) =>
              buf ++= display( hd )

              tl match {
                case NIL =>
                case s: Structure if s.functor == CONS => buf ++= ", "
                case _ => buf ++= " | "
              }

              elems( tl, buf )
            case a =>
              buf ++= display( a )
              buf.toString
          }

        s"[${elems( a )}]"
      case _ => a.toString
    }

}

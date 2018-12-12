package xyz.hyperreal

import scala.collection.mutable.ListBuffer


package object prolog {

  implicit val symbolOrdering = Ordering by Symbol.unapply
  implicit val functorOrdering = Ordering by Functor.unapply
  implicit val procedureOrdering = Ordering by [Procedure, Functor] (_.func)

  case class Functor( name: Symbol, arity: Int )

  case class Procedure( func: Functor, var entry: Int, var end: Int, clauses: ListBuffer[Clause] = new ListBuffer )

  case class Clause( var vars: Int, ast: TermAST )

  case class Structure( functor: Functor, args: Vector[Any] ) extends Product {
    override def productArity = args.length

    override def productElement( n: Int ) = args( n )
  }

  def functor( name: String, arity: Int ) = Functor( Symbol(name), arity )

}
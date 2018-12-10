package xyz.hyperreal

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


package object prolog {

  implicit val symbolOrdering = Ordering by Symbol.unapply
  implicit val functorOrdering = Ordering by Functor.unapply
  implicit val procedureOrdering = Ordering by [Procedure, Functor] (_.func)

  case class Functor( name: Symbol, arity: Int )

  case class Procedure( func: Functor, var entry: Int, var end: Int, clauses: ListBuffer[Clause] = new ListBuffer )

  case class Clause( var vars: Int, ast: TermAST )

  def functor( name: String, arity: Int ) = Functor( Symbol(name), arity )

  class Vars {
    val vars = new mutable.LinkedHashMap[String, Int]

    def count = vars.size

    def num( name: String ) = {
      vars get name match {
        case None =>
          vars(name) = count + 1
          count
        case Some( n ) => n
      }
    }

    def get( name: String ) = vars get name
  }

}
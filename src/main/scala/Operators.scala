package xyz.hyperreal.prolog

import scala.collection.mutable

import xyz.hyperreal.recursive_descent_parser.{Assoc, XFX, XFY, YFX, FX, FY}


object Operators {

  lazy val (optable, opmap) = {
    val _optable = new mutable.HashSet[Operator]
    val _opmap = new mutable.HashMap[(Symbol, Int), Operator]

    def init( priority: Int, specifier: Assoc, operator: Symbol ): Unit = {
      val op = Operator( priority, specifier, operator )

      _optable += op
      _opmap((operator, specifier.name.length - 1)) = op
    }

    init( 1200, XFX, ':- )
    init( 1200, XFX, '--> )
    init( 1200, FX, ':- )
    init( 1200, FX, '?- )
    init( 1100,	XFY, Symbol(";") )
    init( 1050,	XFY, '-> )
    init( 1000,	XFY, Symbol(",") )
    init( 900, FY, Symbol("\\+") )
    init( 700, XFX, Symbol("\\=") )
    init( 700, XFX, '= )
    init( 700, XFX, '== )
    init( 700, XFX, Symbol("\\==") )
    init( 700, XFX, '@< )
    init( 700, XFX, '@=< )
    init( 700, XFX, '@> )
    init( 700, XFX, '@>= )
    init( 700, XFX, Symbol("=..") )
    init( 700, XFX, 'is )
    init( 700, XFX, '=:= )
    init( 700, XFX, '=\= )
    init( 700, XFX, '< )
    init( 700, XFX, '=< )
    init( 700, XFX, '> )
    init( 700, XFX, '>= )
    init( 500, YFX, '+ )
    init( 500, YFX, '- )
    init( 500, YFX, '/\ )
    init( 500, YFX, Symbol("\\/") )
    init( 400, YFX, '* )
    init( 400, YFX, '/ )
    init( 400, YFX, '// )
    init( 400, YFX, 'rem )
    init( 400, YFX, 'mod )
    init( 400, YFX, '<< )
    init( 400, YFX, '>> )
    init( 200, XFX, '** )
    init( 200, XFY, '^ )
    init( 200, FY, '- )
    init( 200, FY, Symbol("\\") )

    (_optable, _opmap)
  }

  def all = optable.iterator

  def priority( p: Int ) = all filter (_.priority == p)

  def specifier( s: Symbol ) = all filter (_.specifier == s)

  def operator( o: Symbol ) = all filter (_.operator == o)

  def defined( op: Symbol, arity: Int ) = opmap contains (op, arity)

  def defined( op: Symbol, specifier: Assoc ): Boolean = defined( op, specifier.name.length - 1 )

  def apply( op: Symbol, arity: Int ) = opmap(op, arity)

  def get( op: Symbol, arity: Int ) = opmap get (op, arity)

  def get( op: Symbol, specifier: Assoc ): Option[Operator] = get( op, specifier.name.length - 1 )

  def add( priority: Int, specifier: Assoc, operator: Symbol ): Unit = {
    if (defined( operator, specifier ))
      sys.error( "operator already defined" )

    val op = Operator( priority, specifier, operator )

    optable += op
    opmap((operator, specifier.name.length - 1)) = op
  }

}
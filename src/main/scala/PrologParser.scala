package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.recursive_descent_parser._

import scala.collection.mutable.ListBuffer


object PrologParser {

  val parser1200 = new ParserRef[TermAST]
  val parser900 = new ParserRef[TermAST]
  val integer = new TokenClassParser( _.isInstanceOf[IntegerToken], (r, s) => IntegerAST(r, s.toInt), "expected integer" )
  val string = new TokenClassParser( _.isInstanceOf[DoubleQuotedToken], (r, s) => StringAST(r, s), "expected string" )
  val cut = Action[(Reader, String), AtomAST]( Parser.symbol("!"), {case (pos, _) => AtomAST(pos, "!")} )
  val anyAtom =
    Alternates(
      List(
        new TokenClassParser( t => t.isInstanceOf[IdentToken] && t.value.head.isLower, (r, s) => AtomAST(r, s), "expected atom" ),
        new TokenClassParser( _.isInstanceOf[SymbolToken], (r, s) => AtomAST(r, s), "expected atom" ),
        new TokenClassParser( _.isInstanceOf[SingleQuotedToken], (r, s) => AtomAST(r, s), "expected atom" )
      ) )
  val variable = new TokenClassParser( t => t.isInstanceOf[IdentToken] && !t.value.head.isLower, {
    case (r, "_") => AnonymousAST( r )
    case (r, s) => VariableAST(r, s) }, "expected variable" )
  val anyNonSymbolAtom =
    Alternates(
      List(
        new TokenClassParser( t => t.isInstanceOf[IdentToken] && t.value.head.isLower, (r, s) => AtomAST(r, s), "expected atom" ),
        new TokenClassParser( _.isInstanceOf[SingleQuotedToken], (r, s) => AtomAST(r, s), "expected atom" )
      ) )
  val primary =
    Alternates[TermAST]( List(
      cut,
      integer,
      string,
      variable,
      Parser.middle( Parser.symbol("("), parser1200, Parser.symbol(")") ),
      Sequence[AtomAST, List[TermAST], StructureAST](
        anyAtom,
        Parser.middle(
          Parser.symbol("("),
          Parser.oneOrMoreSeparated(parser900, Parser.symbol(",")),
          Parser.symbol(")")),
        (atom, args) => StructureAST(atom.pos, atom.name, args) ),
      anyNonSymbolAtom,
      SequenceLeft(
        Sequence[(Reader, String), (List[TermAST], Option[TermAST]), TermAST](
          Parser.symbol("["),
          Sequence[List[TermAST], Option[TermAST], (List[TermAST], Option[TermAST])](
            Parser.oneOrMoreSeparated(parser900, Parser.symbol(",")),
            Optional(SequenceRight(Parser.symbol("|"), parser1200)), (_, _) ), {case (_, (l, t)) => mklist(l, t)} ),
        Parser.symbol("]")),
      Sequence[(Reader, String), (Reader, String), AtomAST]( Parser.symbol("["), Parser.symbol("]"), (a, _) => AtomAST(a._1, "[]") )
    ) )
  var lexer: Lexer = _
  var expression: Parser[TermAST] = _

  build

  def build: Unit = {
    val (rules, ops) =
      Builder[TermAST](
        primary,
        Operators.all.toList map {case Operator(priority, specifier, operator) => Op( priority, specifier, operator.name)},
        (r, s, x) => StructureAST( r, s, List(x) ),
        (x, r, s, y) => StructureAST( r, s, List(x, y) ) )
//      List(
//        Op(1200, XFX, ":-"),
//        Op(1200, XFX, "-->"),
//        Op(1200, FX, ":-"),
//        Op(1200, FX, "?-"),
//        Op(1100, XFY, ";"),
//        Op(1050, XFY, "->"),
//        Op(1000, XFY, ","),
//        Op(900, FY, "\\+"),
//        Op(700, XFX, "="),
//        Op(700, XFX, "\\="),
//        Op( 700, XFX, "==" ),
//        Op( 700, XFX, "\\==" ),
//        Op( 700, XFX, "@<" ),
//        Op( 700, XFX, "@=<" ),
//        Op( 700, XFX, "@>" ),
//        Op( 700, XFX, "@>=" ),
//        Op( 700, XFX, "=.." ),
//        Op( 700, XFX, "is" ),
//        Op( 700, XFX, "=:=" ),
//        Op( 700, XFX, "=\\=" ),
//        Op( 700, XFX, "<" ),
//        Op( 700, XFX, "=<" ),
//        Op( 700, XFX, ">" ),
//        Op( 700, XFX, ">=" ),
//        Op(500, YFX, "+"),
//        Op(500, YFX, "-"),
//        Op(500, YFX, "/\\"),
//        Op(500, YFX, "\\/"),
//        Op(400, YFX, "*"),
//        Op(400, YFX, "/"),
//        Op(400, YFX, "//"),
//        Op(400, YFX, "rem"),
//        Op(400, YFX, "mod"),
//        Op(400, YFX, "<<"),
//        Op(400, YFX, ">>"),
//        Op(200, XFX, "**"),
//        Op(200, XFY, "^"),
//        Op(200, FY, "-"),
//        Op(200, FY, "\\")), (r, s, x) => StructureAST( r, s, List(x) ), (x, r, s, y) => StructureAST( r, s, List(x, y) ) )

    parser1200.ref = rules(1200)
    parser900.ref = rules(900)
    lexer = new Lexer( ops ++ List("(", ")", ".", "[", "]", "|", "!") )
    expression = rules(1200)
  }

  def mklist( ts: List[TermAST], tl: Option[TermAST] ): TermAST =
    ts match {
      case Nil if tl isDefined => tl get
      case Nil => AtomAST( null, "[]" )
      case head :: tail => StructureAST( head.pos, ".", List(head, mklist(tail, tl)) )
    }

  def parseSource( r: Reader ) = {
    val clauses = new ListBuffer[ClauseAST]

    def clause( t: Stream[Token] ): Result[SourceAST] =
      if (t.head.isInstanceOf[EOIToken])
        Success( SourceAST(clauses.toList), t )
      else
        expression( t ) match {
          case Success( result, rest ) =>
            if (rest.head.value == ".") {
              val next =
                result match {
                  case StructureAST( _, ":-", List(StructureAST(pos, "op", List(IntegerAST(_, priority), AtomAST(_, specifier), AtomAST(_, operator)))) ) =>
                    Operators.add( priority, Symbol(specifier), Symbol(operator) )
                    PrologParser.build
                    lexer.tokenStream( rest.tail.head.pos )
                  case _ =>
                    clauses += ClauseAST( result )
                    rest.tail
                }

              clause( next )
            } else
              Failure( s"expected '.': ${rest.head}", rest.tail )
          case f: Failure => f
        }

    clause( lexer.tokenStream(r) )
  }

}
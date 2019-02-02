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
        Sequence[(Reader, String), (List[TermAST], Option[TermAST]), ListAST](
          Parser.symbol("["),
          Sequence[List[TermAST], Option[TermAST], (List[TermAST], Option[TermAST])](
            Parser.oneOrMoreSeparated(parser900, Parser.symbol(",")),
            Optional(SequenceRight(Parser.symbol("|"), parser1200)), (_, _) ), {case ((pos, _), (l, t)) => ListAST(pos, l, t)} ),
        Parser.symbol("]")),
      Sequence[(Reader, String), (Reader, String), AtomAST]( Parser.symbol("["), Parser.symbol("]"), (a, _) => AtomAST(a._1, "[]") )
    ) )
  var lexer: Lexer = _
  var expression: Parser[TermAST] = _

  build

  def build: Unit = {
    val (rules, ops) = Builder[TermAST]( primary,
      List(
        Op(1200, 'xfx, ":-"),
        Op(1200, 'xfx, "-->"),
        Op(1200, 'fx, ":-"),
        Op(1200, 'fx, "?-"),
        Op(1100, 'xfy, ";"),
        Op(1050, 'xfy, "->"),
        Op(1000, 'xfy, ","),
        Op(900, 'fy, "\\+"),
        Op(700, 'xfx, "="),
        Op(700, 'xfx, "\\="),
        Op( 700, 'xfx, "==" ),
        Op( 700, 'xfx, "\\==" ),
        Op( 700, 'xfx, "@<" ),
        Op( 700, 'xfx, "@=<" ),
        Op( 700, 'xfx, "@>" ),
        Op( 700, 'xfx, "@>=" ),
        Op( 700, 'xfx, "=.." ),
        Op( 700, 'xfx, "is" ),
        Op( 700, 'xfx, "=:=" ),
        Op( 700, 'xfx, "=\\=" ),
        Op( 700, 'xfx, "<" ),
        Op( 700, 'xfx, "=<" ),
        Op( 700, 'xfx, ">" ),
        Op( 700, 'xfx, ">=" ),
        Op(500, 'yfx, "+"),
        Op(500, 'yfx, "-"),
        Op(500, 'yfx, "/\\"),
        Op(500, 'yfx, "\\/"),
        Op(400, 'yfx, "*"),
        Op(400, 'yfx, "/"),
        Op(400, 'yfx, "//"),
        Op(400, 'yfx, "rem"),
        Op(400, 'yfx, "mod"),
        Op(400, 'yfx, "<<"),
        Op(400, 'yfx, ">>"),
        Op(200, 'xfx, "**"),
        Op(200, 'xfy, "^"),
        Op(200, 'fy, "-"),
        Op(200, 'fy, "\\")), (r, s, x) => StructureAST( r, s, List(x) ), (x, r, s, y) => StructureAST( r, s, List(x, y) ) )

    parser1200.ref = rules(1200)
    parser900.ref = rules(900)
    lexer = new Lexer( ops ++ List("(", ")", ".", "[", "]", "|", "!") )
    expression = rules(1200)
  }

  def parseSource( r: Reader ) = {
    val clauses = new ListBuffer[ClauseAST]

    def clause( t: Stream[Token] ): Result[SourceAST] =
      if (t.head.isInstanceOf[EOIToken])
        Success( SourceAST(clauses.toList), t )
      else {
          expression( t ) match {
            case Success( result, rest ) =>
              clauses += ClauseAST( result )

              if (rest.head.value == ".")
                clause( rest.tail )
              else
                Failure( s"expected '.': ${rest.head}", rest.tail )
            case f: Failure => f
        }
      }

    clause( lexer.tokenStream(r) )
  }

}
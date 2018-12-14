package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  val code =
    """
      |/*
      |eats(tony,apples).                            // "Tony eats apples"
      |eats(fred,oranges).                           // "Fred eats oranges"
      |eats(fred,t_bone_steaks).                     // "Fred eats T-bone steaks"
      |eats(john,apples).                            // "John eats apples"
      |eats(john,grapefruit).                        // "John eats grapefruit"
      |
      |
      |age(john,32).                 //  John is 32 years old
      |age(agnes,41).                //  Agnes is 41
      |age(george,72).               //  George is 72
      |age(ian,2).                   //  Ian is 2
      |age(thomas,25).               //  Thomas is 25
      |
      |
      |mortal(X) :- human(X).
      |
      |human(socrates).
      |
      |
      |fun(X) :-
      |    red(X),
      |    car(X).
      |
      |fun(X) :-
      |    blue(X),
      |    bike(X).
      |
      |car(vw_beatle).
      |car(ford_escort).
      |bike(harley_davidson).
      |red(vw_beatle).
      |red(ford_escort).
      |blue(harley_davidson).
      |
      |
      |likes(john,mary).
      |likes(john,trains).
      |likes(peter,fast_cars).
      |
      |likes(Person1,Person2):-
      |    hobby(Person1,Hobby),
      |    hobby(Person2,Hobby).
      |
      |hobby(john,trainspotting).
      |hobby(tim,sailing).
      |hobby(helen,trainspotting).
      |hobby(simon,sailing).
      |
      |
      |parent(john,paul).             // paul is john's parent
      |parent(paul,tom).              // tom is paul's parent
      |parent(tom,mary).              // mary is tom's parent
      |
      |ancestor(X,Y) :- parent(X,Y).  // someone is your ancestor if there are your parent
      |ancestor(X,Y) :- parent(X,Z),  // or somebody is your ancestor if they are the parent
      |    ancestor(Z,Y).             // of someone who is your ancestor
      |*/
      |
      |concat( [], L, L ).
      |
      |concat( [E | L1], L2, [E | L3] ) :-
      |  concat( L1, L2, L3 ).
      |
      |append( [], E, [E] ).
      |
      |append( [A | L1], B, [A | L2] ) :- append( L1, B, L2 ).
      |
      |
      |//go( a(b, c) ).
    """.stripMargin
  val query =
    """
      |concat( [3], [4], L )
    """.stripMargin
  val prog = new Program

  Parser.source( new StringReader(code) ) match {
    case Parser.Match( ast, _ ) =>
      //println( ast )
      Compiler.compile( ast, prog )
      //prog.print
    case m: Parser.Mismatch => m.error
  }

  Parser.query( new StringReader(query) ) match {
    case Parser.Match( ast, _ ) =>
      //println( ast )

      val vm = new VM( prog ) //{trace = true}

      println( vm.interpall(ast) map (_.map { case (k, v) => k -> display(v)}) )
    case m: Parser.Mismatch => m.error
  }

}
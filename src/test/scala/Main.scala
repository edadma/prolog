package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  val code =
    """
      |quick_sort(List,Sorted):-qsort(List,Sorted,[]).
      |
      |qsort([], R, R).
      |qsort([X|L], R, R0) :-
      |	partition(L, X, L1, L2),
      |	qsort(L2, R1, R0),
      |	qsort(L1, R, [X|R1]).
      |
      |partition([],_,[],[]).
      |partition([X|L],Y,[X|L1],L2) :-
      |	X =< Y, !,
      |	partition(L,Y,L1,L2).
      |partition([X|L],Y,L1,[X|L2]) :-
      |	partition(L,Y,L1,L2).
    """.stripMargin
  val query =
    """
      |quick_sort( [3, 2, 1], L )
    """.stripMargin
  val prog = new Program

  println( "parsing program" )

  Parser.source( new StringReader(code) ) match {
    case Parser.Match( ast, _ ) =>
      //println( ast )
      println( "compiling program" )
      Compiler.debug = true
      Compiler.compile( ast, prog )
      //prog.print

      println( "parsing query" )

      Parser.query( new StringReader(query) ) match {
        case Parser.Match( ast, _ ) =>
          //println( ast )

          println( "interpreting query" )

          val vm = new VM( prog ) {trace = false; debug = false/*; out = new PrintStream( "debug" )*/}

          //println( vm.interpall(ast) map (_.map { case (k, v) => k -> display(v)}) )
          println( vm.interp(ast) map (_.map { case (k, v) => k -> display(v)}) )
        case m: Parser.Mismatch => m.error
      }
    case m: Parser.Mismatch => m.error
  }

}

/*
eats(tony,apples).                            // "Tony eats apples"
eats(fred,oranges).                           // "Fred eats oranges"
eats(fred,t_bone_steaks).                     // "Fred eats T-bone steaks"
eats(john,apples).                            // "John eats apples"
eats(john,grapefruit).                        // "John eats grapefruit"


age(john,32).                 //  John is 32 years old
age(agnes,41).                //  Agnes is 41
age(george,72).               //  George is 72
age(ian,2).                   //  Ian is 2
age(thomas,25).               //  Thomas is 25


mortal(X) :- human(X).

human(socrates).


fun(X) :-
    red(X),
    car(X).

fun(X) :-
    blue(X),
    bike(X).

car(vw_beatle).
car(ford_escort).
bike(harley_davidson).
red(vw_beatle).
red(ford_escort).
blue(harley_davidson).


likes(john,mary).
likes(john,trains).
likes(peter,fast_cars).

likes(Person1,Person2):-
    hobby(Person1,Hobby),
    hobby(Person2,Hobby).

hobby(john,trainspotting).
hobby(tim,sailing).
hobby(helen,trainspotting).
hobby(simon,sailing).


parent(john,paul).             // paul is john's parent
parent(paul,tom).              // tom is paul's parent
parent(tom,mary).              // mary is tom's parent

ancestor(X,Y) :- parent(X,Y).  // someone is your ancestor if there are your parent
ancestor(X,Y) :- parent(X,Z),  // or somebody is your ancestor if they are the parent
    ancestor(Z,Y).             // of someone who is your ancestor
*/
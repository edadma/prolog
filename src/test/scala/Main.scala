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
      |/*
      |queens(N,Qs):-
      |	range(1,N,Ns),
      |	queens(Ns,[],Qs).
      |
      |queens([],Qs,Qs).
      |
      |queens(UnplacedQs,SafeQs,Qs):-
      |	sel(UnplacedQs,UnplacedQs1,Q),
      |	not_attack(SafeQs,Q),
      |	queens(UnplacedQs1,[Q|SafeQs],Qs).
      |
      |not_attack(Xs,X):-
      |	not_attack(Xs,X,1).
      |
      |not_attack([],_,_).
      |
      |not_attack([Y|Ys],X,N):-
      |	X =\= Y+N,
      |	X =\= Y-N,
      |	N1 is N+1,
      |	not_attack(Ys,X,N1).
      |
      |sel([X|Xs],Xs,X).
      |
      |sel([Y|Ys],[Y|Zs],X):-
      |	sel(Ys,Zs,X).
      |
      |range( N, N, [N] ) :- !.
      |
      |range( M, N, [M | Ns] ) :-
      |	M < N,
      |	M1 is M + 1,
      |	range( M1, N, Ns ).
      |*/
      |
      |quick_sort(List,Sorted):-q_sort(List,[],Sorted).
      |
      |q_sort([],Acc,Acc).
      |q_sort([H|T],Acc,Sorted):-
      |	pivoting(H,T,L1,L2),
      |	q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted).
      |
      |pivoting(H,[],[],[]).
      |pivoting(H,[X|T],[X|L],G):-X>=H,pivoting(H,T,L,G).
      |pivoting(H,[X|T],L,[X|G]):-X<H,pivoting(H,T,L,G).
      |
      |go( R ) :- quick_sort( [3, 2, 5, 4], R ).
      |
      |//go :- (1 = 2 -> write( yes ), nl ; write( no ), nl), write( after ), nl.
    """.stripMargin
  val query =
    """
      |go( R )
      |
      |//queens( 4, Qs )
    """.stripMargin
  val prog = new Program

  /*
  | | |x| |
  |x| | | |
  | | | |x|
  | |x| | |
   */
  Parser.source( new StringReader(code) ) match {
    case Parser.Match( ast, _ ) =>
      //println( ast )
      Compiler.debug = true
      Compiler.compile( ast, prog )
      //prog.print

      Parser.query( new StringReader(query) ) match {
        case Parser.Match( ast, _ ) =>
          //println( ast )

          val vm = new VM( prog ) {trace = false; debug = false}

          println( vm.interpall(ast) map (_.map { case (k, v) => k -> display(v)}) )
        //      println( vm.interp(ast) map (_.map { case (k, v) => k -> display(v)}) )
        case m: Parser.Mismatch => m.error
      }
    case m: Parser.Mismatch => m.error
  }

}
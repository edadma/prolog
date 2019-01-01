/* non ISO  */

writeln( A ) :- write( A ), nl.


/* Term IO */

write( Term ) :-
  current_output( S ),
  write_term( S, Term, [numbervars(true)] ).

read( Term ) :-
  current_input( S ),
  read_term( S, Term, [] ).

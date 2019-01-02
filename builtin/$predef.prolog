/* non ISO  */

writeln( A ) :- write( A ), nl.

read_string( String ) :-
  current_input( S ),
  read_string( S, String ).

/* Term IO */

write( Term ) :-
  current_output( S ),
  write_term( S, Term, [numbervars(true)] ).

read( Term ) :-
  current_input( S ),
  read_term( S, Term, [] ).

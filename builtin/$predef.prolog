/*
    non ISO predicates
*/

writeln( A ) :- write( A ), nl.

write( Term ):-
  current_output( S ),
  write_term( S, Term, [numbervars(true)] ).

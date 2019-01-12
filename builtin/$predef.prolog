/* non ISO  */

writeln( A ) :- write( A ), nl.

read_string( String ) :-
  current_input( S ),
  read_string( S, String ).


/* Term IO */

write_term( Term, Options ) :-
  current_output( S ),
  write_term( S, Term, Options ).

write( Term ) :-
  current_output( S ),
  write_term( S, Term, [numbervars(true)] ).

write_term( S, Term ) :-
  write_term( S, Term, [numbervars(true)] ).

writeq( Term ) :-
  current_output( S ),
  write_term( S, Term, [quoted(true), numbervars(true)] ).

writeq( S,Term ) :-
  write_term( S, Term, [quoted(true), numbervars(true)] ).

write_canonical( T ) :-
  current_output( S ),
  write_term( S, Term, [quoted(true), ignore_ops(true)] ).

write_canonical( S,T ) :-
  write_term( S, Term, [quoted(true), ignore_ops(true)] ).

read_term( Term, Options ) :-
  current_input( S ),
  read_term( S, Term, Options ).

read( S, Term ) :-
  read_term( S, Term, [] ).

read( Term ) :-
  current_input( S ),
  read_term( S, Term, [] ).


/* Character IO */

get_char( Char ) :-
    current_input( S ),
    get_char( S, Char ).

get_code( Code ) :-
    current_input( S ),
    get_code( S, Code ).

get_code( S, Code ) :-
    get_char( S, Char ),
    (Char = end_of_file ->
        Code = -1
    ;
        char_code( Char, Code )
    ).

put_code( Code ) :-
  current_output( S ),
  char_code( Char, Code ),
  put_char( S, Char ).

put_code( S, Code ) :-
  char_code( Char, Code ),
  put_char( S, Char ).

put_char( Char ) :-
  current_output( S ),
  put_char( S, Char ).

nl :-
  put_char( '\n' ).

nl( S ) :-
  put_char( S, '\n' ).


/* Binary IO */

get_byte( byte ) :-
    current_input( S ),
    get_byte( S, Byte ).

put_byte( byte ) :-
    current_output( S ),
    put_byte( S, Byte ).


/* Stream Selection */

open( Source_sink, Mode, Stream ) :-
    open( Source_sink, Mode, Stream, [] ).

flush_output :-
    current_output( S ),
    flush_output( S ).

close( S ) :-
    close( S, [] ).
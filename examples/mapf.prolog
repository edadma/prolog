mapf( F, [], [] ).
mapf( F, [H | T], [A | B] ) :- S =.. [F, H], A is S, mapf( F, T, B ).
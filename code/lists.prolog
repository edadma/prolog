append( [], L, L ).
append( [H | L1], L2, [H | L3] ) :- append( L1, L2, L3 ).

prefix( X, Z ) :- append( X, _, Z ).

suffix( Y, Z ) :- append( _, Y, Z ).

member( T, [T | _] ).
member( X, [_ | Q] ) :- member( X, Q ).

reverse( List, Reversed ) :- reverse( List, [], Reversed ).
reverse( [], Reversed, Reversed ).
reverse( [Head | Tail], SoFar, Reversed ) :- reverse( Tail, [Head | SoFar], Reversed ).

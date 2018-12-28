mov( 1, 2 ). mov( 1, -2 ). mov( -1, 2 ). mov( -1, -2 ).
mov( 2, 1 ). mov( 2, -1 ). mov( -2, 1 ). mov( -2, -1 ).

jump( pos(X0, Y0), pos(X1, Y1) ) :-
  mov( X, Y ),
  X1 is X0 + X,
  Y1 is Y0 + Y,
  X1 >= 1, X1 =< 5,
  Y1 >= 1, Y1 =< 5.

tour( Init, Tour ) :-
  tour( Init, [Init], 1, Tour ).

tour( _, Visited, 25, Visited ).
tour( Position, Visited, N, Tour ) :-
  jump( Position, Next ),
  \+ member( Next, Visited ),
  M is N + 1,
  tour( Next, [Next | Visited], M, Tour ).

member( T, [T | _] ).
member( X, [_ | Q] ) :- member( X, Q ).
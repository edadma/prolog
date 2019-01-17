append( [], L, L ).
append( [H | L1], L2, [H | L3] ) :-
    append( L1, L2, L3 ).

prefix( [], _ ).
prefix( [E | T0], [E | T] ) :-
    prefix( T0, T ).

suffix( Y, Z ) :-
    append( _, Y, Z ).

nextto( X, Y, [X, Y | _] ).
nextto( X, Y, [_ | Zs] ) :-
    nextto( X, Y, Zs ).

member( T, [T | _] ).
member( X, [_ | Q] ) :-
    member( X, Q ).

reverse( List, Reversed ) :-
    reverse( List, [], Reversed ).

reverse( [], Reversed, Reversed ).
reverse( [Head | Tail], SoFar, Reversed ) :-
    reverse( Tail, [Head | SoFar], Reversed ).

select( X, [X | Xs], Xs ).
select( X, [Y | Ys], [Y | Zs] ) :-
    select( X, Ys, Zs ).

length( Xs, L ) :- length( Xs, 0, L ) .

length( [], L, L ).
length( [_ | Xs], T, L ) :-
  T1 is T + 1,
  length( Xs, T1, L ).

permutation( [], [] ).
permutation( List, [First | Perm] ) :-
    select( First, List, Rest ),
    permutation( Rest, Perm ).

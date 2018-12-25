female( anne ).
female( rosie ).
female( emma ).
female( olivia ).
female( mia ).

male( randy ).
male( don ).
male( liam ).
male( logan ).
male( aiden ).

parent( don, randy ).
parent( don, anne ).
parent( rosie, randy ).
parent( rosie, anne ).
parent( liam, don ).
parent( olivia, don ).
parent( liam, mia ).
parent( olivia, mia ).
parent( emma, rosie ).
parent( logan, rosie ).
parent( emma, aiden ).
parent( logan, aiden ).

relation( X, Y ) :- ancestor( A, X ), ancestor( A, Y ), X \= Y.
ancestor( X, Y ) :- parent( X, Y ) ; parent( X, P ), ancestor( P, Y ).

mother( X, Y ) :- female( X ), parent( X, Y ).
father( X, Y ) :- male( X ), parent( X, Y ).
daughter( X, Y ) :- female( X ), parent( Y, X ).
son( X, Y ) :- male( X ), parent( Y, X ).
siblings( X, Y ) :- parent( P, X ), parent( P, Y ), X \= Y.
full_siblings( A, B ) :-
  parent( F, A ), parent( F, B ),
  parent( M, A ), parent( M, B ),
  A \= B, F \= M.
sister( X, Y ) :- female( X ), siblings( Y, X ).
brother( X, Y ) :- male( X ), siblings( Y, X ).
uncle( U, N ) :- male( U ), siblings( U, P ), parent( P, N ).
aunt( A, N ) :- female( A ), siblings( A, P ), parent( P, N ).
grandparent( X, Y ) :- parent( X, P ), parent( P, Y ).
grandmother( X, Y ) :- female( X ), grandparent( X, Y ).
grandfather( X, Y ) :- male( X ), grandparent( X, Y ).

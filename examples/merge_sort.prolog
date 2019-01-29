merge_sort( [], [] ).     % empty list is already sorted
merge_sort( [X], [X] ).   % single element list is already sorted
merge_sort( List, Sorted ):-
    List = [_,_|_], halve( List, L1, L2 ),				% list with at least two elements is divided into two parts
    merge_sort( L1, Sorted1 ), merge_sort( L2, Sorted2 ),	% then each part is sorted
    merge( Sorted1, Sorted2, Sorted ).					% and sorted parts are merged

merge( [], L, L ).
merge( L, [], L ) :- L \= [].
merge( [X|T1], [Y|T2], [X|T] ) :- X =< Y, merge( T1, [Y|T2], T ).
merge( [X|T1], [Y|T2], [Y|T] ) :- X > Y, merge( [X|T1], T2, T ).

halve( L, A, B ) :- halve_( L, L, A, B ).

halve_( [], R, [], R ).   % for lists of even length
halve_( [_], R, [], R ).  % for lists of odd length
halve_( [_, _|T], [X|L], [X|L1], R ) :- halve_( T, L, L1, R ).

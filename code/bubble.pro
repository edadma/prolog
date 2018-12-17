bubble(List, Sorted) :-
    append(InitList, [B,A|Tail], List),
    A < B,
    append(InitList, [A,B|Tail], NewList),
    bubble(NewList, Sorted).
bubble(List, List).
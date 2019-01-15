fib(C, P,S, C, N)  :- !, N is P + S.
fib(C, P,S, Cv, V) :- Cn is C + 1, N is P + S, fib(Cn, S,N, Cv, V).

fib(0, 0).
fib(1, 1).
fib(C, N) :- fib(2, 0,1, C, N).
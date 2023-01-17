factor(N, I, true):- I mod N #= 0.
factor(N, I, false):- I mod N #\= 0.
factors(N, L, R):- tfilter(factor(N), L, R).
union(Ls, U) :- foldl(union, Ls, [], U).

pe1(F, N, R):-
    numlist(1, N, L0),
    maplist([X]>>(factors(X,L0)),F,L1),
    union(L1,L2),
    sum_list(L2,R).

answer:-
    pe1([3,5],999,O),
    writeln(O).
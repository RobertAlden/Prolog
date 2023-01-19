factor(N, I, true):- I mod N #= 0.
factor(N, I, false):- I mod N #\= 0.

pe1(Fs, N, R):-
    numlist(1, N, L0),
    maplist([X]>>(tfilter(factor(X),L0)),Fs,L1),
    foldl(union, L1, [], L2),
    sum_list(L2,R).

answer:-
    pe1([3,5],999,O),
    writeln(O).
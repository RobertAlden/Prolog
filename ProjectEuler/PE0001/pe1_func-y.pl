:- use_module(rob_utils).
:- use_module(func_utils).

factor(N, I, true):- I mod N #= 0.
factor(N, I, false):- I mod N #\= 0.

pe1(Fs, N, R):-
    naturals(N, L0),
    Fs~>maplist([X]>>(tfilter(factor(X),L0)))~>
    fold_(union, [])~>reduce(+)~>R.

answer:-
    pe1([3,5],999,O),
    writeln(O).
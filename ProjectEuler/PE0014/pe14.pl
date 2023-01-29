:- use_module(rob_utils, [numlist_/3, reverse_/3]).
:- table collatz_next/2.
:- table collatz/2.
:- table collatz/3.

collatz_next(N0,N) :-
    R #= N0 mod 2,
    if_(R=0, N0 #= 2*N, N #= 3*N0 + 1).

collatz_reaches(N,N).
collatz_reaches(N0,N) :-
    collatz_next(N0,N1),
    collatz_reaches(N1,N).

collatz(N,Cl) :-
    zcompare(C,N,1),
    collatz(C,N,Cl).

collatz(>,N0,[N0|Cl]) :-
    collatz_next(N0,N1),
    collatz(N1,Cl).
collatz(=,1,[1]).

pe14(A,B,R) :-
    numlist_(A,B,L0),
    phrase(reverse_(L0),L1),
    maplist(collatz,L1,L2), 
    maplist(length,L2,L3),
    max_list(L3,M),
    nth0(R0,L3,M),
    nth0(R0,L1,R).
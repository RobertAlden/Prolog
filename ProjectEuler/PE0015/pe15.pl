:- use_module(rob_utils, [numlist_/3]).
:- use_module(func_utils).

pe15(N,R) :-
    N1 #= N*2 + 1,
    N2 #= N*2 + 2,
    numlist_(1,N1,L0),
    power(N2,commute(scan_(plus,0)),L0,L1),
    nth0(N,L1,R).
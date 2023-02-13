:- use_module(rob_utils).
:- use_module(func_utils).

pe15(N,R) :-
    N1 #= N*2 + 1,
    N2 #= N*2 + 2,
    naturals(N1)~>power(N2,commute(scan(+)))~>nth0(N)~>R.
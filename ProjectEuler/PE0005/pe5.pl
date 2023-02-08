:- use_module(rob_utils).
:- use_module(func_utils).

pe5(Limit, Result) :-
    Limit>-numlist_(1)>-fold_(lcm_,1)>-Result.

answer :-
    pe5(20,R),
    writeln(R).
:- use_module(rob_utils).
:- use_module(func_utils).

pe6(Limit,Result):-
    naturals(Limit,L),
    maplist([X,Y]>>(Y #= X*X),L,L0),
    sum_list(L0,SuSq),
    sum_list(L,Su),
    SqSu #= Su * Su,
    Result #= SqSu - SuSq.

answer :-
    pe6(100,R),
    writeln(R).
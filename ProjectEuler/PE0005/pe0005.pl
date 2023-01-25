:- use_module(rob_utils, [numlist_/3, lcm_/3]).

pe5(Limit, Result) :-
    numlist_(1,Limit, L),
    foldl(lcm_, L, 1, Result).

answer :-
    pe5(20,R),
    writeln(R).
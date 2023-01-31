:- use_module(rob_utils, [n_factorial/2, digits_/2]).
answer :-
    n_factorial(100,X),
    digits_(X,Ds),
    sum_list(Ds,R),
    writeln(R).


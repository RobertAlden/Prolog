:- use_module(rob_utils).
:- use_module(func_utils).

pe20(X,R) :-
    n_factorial(X)~>number_digits~>reduce(+)~>R.


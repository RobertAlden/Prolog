:- use_module(rob_utils).
:- use_module(func_utils).
:- use_module(prime_utils).
answer:-
    prime_factors(600851475143)~>max_list~>Result,
    writeln(Result).

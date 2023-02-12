:- use_module(rob_utils).
:- use_module(func_utils).
:- use_module(prime_utils).
answer:-
    600851475143~>prime_factors~>max_list~>Result,
    writeln(Result).

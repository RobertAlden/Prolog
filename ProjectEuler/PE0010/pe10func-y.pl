:- use_module(prime_utils).
:- use_module(rob_utils).
:- use_module(func_utils).

answer:-
    2_000_000~>prime_sieve~>reduce(+)~>Sum,
    writeln(Sum).
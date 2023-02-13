:- use_module(prime_utils).
:- use_module(rob_utils).
:- use_module(func_utils).

answer:-
    prime_sieve(2_000_000)~>reduce(+)~>Sum,
    writeln(Sum).
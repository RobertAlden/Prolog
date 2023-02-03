:- use_module(prime_utils, [prime_sieve/2]).

answer:-
    prime_sieve(2_000_000, Pr),
    sum_list(Pr, Sum),
    writeln(Sum).
:- use_module(rob_utils, [primes/2]).

pe7(N,R):-
    Nz #= 11*N,
    primes(Nz,P),
    nth0(N,P,R).

answer:-
    pe7(10000,R),
    writeln(R).

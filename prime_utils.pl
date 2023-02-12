%% Prime number utils
:- module(prime_utils, [prime_sieve/2,
                        prime_factors/2,
                        is_prime/1]).
:-table prime_sieve/3.
:-use_module(rob_utils).
% Sieve of Eratosthenes:
prime_sieve(N, Primes) :- 
    step_range(2, 3, N, Xs),
    sieve(N,Xs, Pl),
    append([2],Pl,Primes).
div_(B,A) :- A mod B #= 0.
sieve(_,[],[]).
sieve(N,[L|Ls],[L|Rs]) :-
    psqrt_(N,SqrtN),
    RSqrtN is round(SqrtN),
    L #=< RSqrtN,
    exclude(div_(L),Ls,Ls1),
    sieve(N,Ls1,Rs).
sieve(N,[L|Ls],[L|Ls]) :-
    psqrt_(N,SqrtN),
    RSqrtN is round(SqrtN),
    L #> RSqrtN.

prime_factors(N, Fs):-
    once(prime_factors(2, N, [], Fs0)),
    reverse(Fs0, Fs).
prime_factors(_, 1, R, R).
prime_factors(D,N,Acc,[N|Acc]):-
    psqrt_(N,SqrtN),
    RSqrtN is round(SqrtN),
    D #> RSqrtN.
prime_factors(D,N,Acc,Fs):-
    divmod(N,D,Q,R),
    Dn #= D+1,
    N #> 1,
    if_(R=0,prime_factors(D,Q,[D|Acc],Fs), prime_factors(Dn,N,Acc,Fs)).

is_prime(X) :- prime_factors(X,[X]).
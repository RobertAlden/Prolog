%% Prime number utils
:- module(prime_utils, [prime_sieve/2,
                        prime_factors/2]).
:-table prime_sieve/2.
% Sieve of Eratosthenes:
prime_sieve(N, Pl) :- 
    numlist(2, N, Xs),
    sieve(Xs, Pl).
div_(B,A) :- A mod B #= 0.
sieve([],[]).
sieve([L|Ls],[L|Rs]) :- 
    exclude(div_(L),Ls,Ls1),
    sieve(Ls1,Rs).


prime_factors(N, Fs):-
    prime_factors(2, N, [], Fs0),
    reverse(Fs0, Fs).
prime_factors(_, 1, R, R).
prime_factors(D,N,Acc,Fs):-
    divmod(N,D,Q,R),
    Dn #= D+1,
    N #> 1,
    (R=0 -> prime_factors(D,Q,[D|Acc],Fs); prime_factors(Dn,N,Acc,Fs)).

list_to_product(L,Product) :- foldl([X,FL,TR]>>(TR is X*FL),L,1,Product).

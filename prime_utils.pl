:- include(rob_utils).

% Sieve of Eratosthenes:
prime_sieve(N, L) :- numlist(2, N, Xs),
            sieve(Xs, L).

sieve([H|T], [H|X]) :- H2 is H + H, 
                       filter(H, H2, T, R),
                       sieve(R, X).
sieve([], []).

filter(_, _, [], []).
filter(H, H2, [H1|T], R) :- 
    (H1 < H2 -> R = [H1|R1], filter(H, H2, T, R1);
     H3 is H2 + H,
    (H1 =:= H2 -> filter(H, H3, T, R);
                  filter(H, H3, [H1|T], R)
    )).

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


is_prime_(2,true).
is_prime_(3,true).
is_prime_(X,true) :- 
    X mod 2 #= Z,
    X mod 3 #= Z,
    Z #\= 0,
    (X #= 6*K+1 #\/ X #= 6*K+5),
    psqrt_(X,L),
    floor(L,Lf),
    K in 1..Lf,
    indomain(K).
is_prime_(1,false).
is_prime_(X,false) :- not(is_prime_(X,true)).
:- module(rob_utils, [reverse_/3,
                      last_/2,
                      incrementing_/1,
                      ascending_/1,
                      is_factor_of/3,
                      is_odd/1,
                      is_even/1,
                      rp/3,
                      pack_together/2
                     ]).

reverse_([]) --> [].
reverse_([L|Ls]) --> reverse_(Ls), [L].

% Is X the last item of the list Z?
last_(X,Z) :- append(_,[X],Z).

% Rewrote numlist to be reversible.
numlist_(X,Y,Z) :- 
   X #=< Y,
   S #= Y - X + 1,
   S1 #= Y - X,
   length(Z,S),
   nth0(0, Z, X),
   nth0(S1, Z, Y),
   incrementing_(Z),
   ascending_(Z).

incrementing_([]).
incrementing_([_]).
incrementing_([Z1,Z2|Zs]) :- 
   Z2 #= Z1 + 1,
   incrementing_([Z2|Zs]).

ascending_([]).
ascending_([_]).
ascending_([Z1,Z2|Zs]) :- 
   Z1 #< Z2,
   ascending_([Z2|Zs]).

% playing with some predicates
sqrt_(X,R) :- {X = R*R}.


is_factor_of(F, N, 0):- N mod F #\= 0.
is_factor_of(F, N, 1):- N mod F #= 0.

has_factors(N, Fs):-
    are_factors_of(Fs, N),
    ascending_(Fs).

are_factors_of([X,Y],N) :- X*Y #= N, X #> 0, Y #>0, labeling(['ff'],[X,Y]).
are_factors_of([F0|Fs],N):- 
    last_(Fn, Fs),
    F0 * Fn #= N,
    N #> 0,
    append(NFs,[Fn],Fs),
    are_factors_of(NFs,N). 

prime_factors(N, Fs):-
    prime_factors(2, N, [], Fs0),
    reverse(Fs0, Fs).
prime_factors(_, 1, R, R).
prime_factors(D,N,Acc,Fs):-
    divmod(N,D,Q,R),
    Dn #= D+1,
    N #> 1,
    (R=0 -> prime_factors(D,Q,[D|Acc],Fs); prime_factors(Dn,N,Acc,Fs)).

is_prime_(N) :- has_factors(N,[1,N]).

is_even(N) :- is_factor_of(2,N,1).

is_odd(N):- not(is_even(N)).

rp(Pred, Arg, 1) :- call(Pred, Arg).
rp(Pred, Arg, 0) :- not(call(Pred, Arg)).
rp(Pred, Arg, true) :- call(Pred, Arg).
rp(Pred, Arg, false) :- not(call(Pred, Arg)).

% Factorial
n_factorial(N, F) :-
        zcompare(C, N, 0),
        n_factorial_(C, N, F).

n_factorial_(=, _, 1).
n_factorial_(>, N, F) :-
        F #= F0*N,
        N1 #= N - 1,
        n_factorial(N1, F0).

count(L, E, N) :-
    include(=(E), L, L2), length(L2, N).

% Sieve of Eratosthenes:
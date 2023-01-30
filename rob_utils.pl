:- module(rob_utils, [reverse_/3,
                      last_/2,
                      numlist_/3,
                      incrementing_/1,
                      ascending_/1,
                      is_factor_of/3,
                      is_odd/1,
                      is_even/1,
                      rp/2,
                      fetch_file/2,
                      palindrome/1,
                      n_factorial/2,
                      gcd_/3,
                      lcm_/3,
                      pack_together/2,
                      primes/2,
                      is_prime/2,
                      mod_/3,
                      modn_/3,
                      windowed/3,
                      digits_/2
                     ]).

reverse_([]) --> [].
reverse_([L|Ls]) --> reverse_(Ls), [L].

palindrome(X) :- phrase(reverse_(X),X).

all([])     --> [].
all([L|Ls]) --> [L], all(Ls).

fetch_file(F, R):-
    once(phrase_from_file(all(Ls), F)),
    maplist([X,Y]>>(char_code(Y,X)),Ls,C), % this is wild.
    string_chars(Cs,C), % what string Cs relates to the list of chars C? Prolog knows!
    split_string(Cs, '\n', '\s\t', R). % split that string on \n, removing \s\t padding from the resulting substrings.

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
   incrementing_(Z).

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

% {} means clp(r) - constraint logic programming over reals
sqrt_(X,R) :- {X = R*R}.
psqrt_(X,R) :- {X = R*R, R >= 0}.

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

mod_(X,Y,Truth) :- integer(X), integer(Y), !, ( X mod Y =:= 0-> Truth=true ; Truth=false ).
mod_(X,Y,true) :- X mod Y #= 0.
mod_(X,Y,false) :- X mod Y #>= 0.

modn_(X,Y,Truth) :- integer(X), integer(Y), !, ( X mod Y > 0-> Truth=true ; Truth=false ).
modn_(X,Y,false) :- X mod Y #= 0.
modn_(X,Y,true) :- X mod Y #> 0.



% help function to reify predicates.
rp(PredArgs, true) :- 
    C =..[call|PredArgs], C.
rp(PredArgs, false) :- 
    C =..[call|PredArgs], not(C).


% Factorial
n_factorial(N, F) :-
        zcompare(C, N, 0),
        n_factorial_(C, N, F).

n_factorial_(=, _, 1).
n_factorial_(>, N, F) :-
        F #= F0*N,
        N1 #= N - 1,
        n_factorial(N1, F0).

% GCD
gcd_(Gcd,0,Gcd).
gcd_(A,B,Gcd) :-
    Bn #= A mod B,
    gcd_(B,Bn,Gcd).

% LCM
lcm_(A,B,C) :-
    gcd_(A,B,Gcd),
    C #= A * B // Gcd.

count(L, E, N) :-
    include(=(E), L, L2), length(L2, N).
pack_together(Ls,Rs):-
    maplist(count(Ls),Ls,Rs).

% Sieve of Eratosthenes:
primes(N, L) :- numlist(2, N, Xs),
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

is_prime(1,false).
is_prime(2,true).
is_prime(3,true).
is_prime(5,true).
is_prime(X,false) :- X mod 2 #= 0, X #\= 2.
is_prime(N,T) :-
    numlist_(2,N-1,Cl),
    tfilter([X]>>(mod_(N,X)),Cl,Fl),
    (length(Fl,0) -> T=true ; T=false ).

prime_list(A,B,C) :-
    numlist_(A,B,L0),
    once(tfilter(is_prime,L0,C)).

next_prime(2,3).
next_prime(3,5).
next_prime(X,P) :-
    X1 #= X+1,
    X2 #= X*2,
    between(X1,X2,P),
    is_prime(P,true),
    labeling([min(1)],[P]),
    indomain(P).

windowed(L,N,R):-
    N #> 0,
    length(L,Ne),
    zcompare(C,N,Ne),
    windowed(C,L,N,R).
windowed(<,[L|Ls],N,[Ln|Ws]):-
    length(Ln,N),
    append(Ln,_,[L|Ls]),
    windowed(Ls,N,Ws).
windowed(=,L,N,[L]):-
    length(L,N).

conv(X,Y) :- X #= Y + 48.
digits_(St,Ds) :-
    number_codes(St,Cs),
    maplist(conv,Cs,Ds).

composite(N,true) :-
    N #= A*_,
    psqrt_(N,S),
    floor(S,Sf),
    A in 2..Sf,
    indomain(A).

composite(N,false) :- not(composite(N,true)).

mi([]).
mi([G|Gs]) :- head_body_(G, Goals, Gs), mi(Goals).
head_body_(mi([]), Rs, Rs).
head_body_(mi([G|Gs]), [head_body_(G,Goals,Gs), mi(Goals)|Rs], Rs).
head_body_(head_body_(Head, Goals0, Goals), Rs, Rs) :-
    head_body_(Head, Goals0, Goals).
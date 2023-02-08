%% Catch-all utilities file until i make enough
%% to make it worth breaking it off into a stand-alone
:- module(rob_utils, [numlist_/3,
                      reverse_/2,
                      last_/2,
                      numlist_/3,
                      incrementing_/1,
                      ascending_/1,
                      rp/2,
                      fetch_file/2,
                      palindrome/1,
                      n_factorial/2,
                      gcd_/3,
                      lcm_/3,
                      pack_together/2,
                      mod_/3,
                      modn_/3,
                      windowed/3,
                      number_digits/2
                     ]).

reverse_([]) --> [].
reverse_([L|Ls]) --> reverse_(Ls), [L].
reverse_(L,R) :- phrase(reverse_(L),R).

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

number_digits(Num,Dig) :-
    once(digits_(Num,Dig)).

digits_(St,Ds) :-
    var(Ds),
    number_codes(St,Cs),
    maplist([X,Y]>>(X #= Y + 48),Cs,Ds).
digits_(St,Ds) :-
    var(St),
    maplist([X,Y]>>(X #= Y + 48),Cs,Ds),
    number_codes(St,Cs).
    

reverse_([]) --> [].
reverse_([L|Ls]) --> reverse_(Ls), [L].

fibonacci(N,F,Cache) :-
   M is N+1,
   length(Cache,M),
   fibonacci(Cache),
   Cache=[F|_].

fibonacci([0]).
fibonacci([1,0]).
fibonacci([F,FA,FB|More]) :-
   F #= FA+FB,
   fibonacci([FA,FB|More]).

numlist_(X,Y,Z) :- 
   S #= Y - X + 1,
   S1 #= Y - X,
   length(Z,S),
   nth0(0, Z, X),
   nth0(S1, Z, Y),
   ascending_(Z).

ascending_([]).
ascending_([_]).
ascending_([Z1,Z2|Zs]) :- 
   succ(Z1, Z2),
   ascending_([Z2|Zs]).

is_odd(X, true) :- X mod 2 #= 1.
is_odd(X, false) :- not(is_odd(X, true)).
is_even(X, true) :- is_odd(X, false).
is_even(X, false) :- is_odd(X, true).

odd_list(X,Y,R) :- 
   numlist_(X,Y,L),
   tfilter(is_odd, L, R).


robert(X) :- X mod 5 #= 0.
%% is_robert(X,true) :- robert(X).
%% is_robert(X,false) :- not(robert(X)).
r(Pred, Arg, true) :- call(Pred, Arg).
r(Pred, Arg, false) :- not(call(Pred, Arg)).
% DCG that reverses a list
reverse_([]) --> [].
reverse_([L|Ls]) --> reverse_(Ls), [L].

% Is X the last item of the list Z?
last_(X,Z) :- append(_,[X],Z).
% What about the last two items of Z, X and Y?
last2_(X,Y,Z) :- append(_,[X,Y],Z).


% Cool fibonacci algorithm
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
is_odd(X, true) :- X mod 2 #= 1.
is_odd(X, false) :- not(is_odd(X, true)).
is_even(X, true) :- is_odd(X, false).
is_even(X, false) :- is_odd(X, true).

odd_list(X,Y,R) :- 
   numlist_(X,Y,L),
   tfilter(is_odd, L, R).


robert(X) :- X mod 5 #= 0.
r(Pred, Arg, true) :- call(Pred, Arg).
r(Pred, Arg, false) :- not(call(Pred, Arg)).

rob_list(X,Y,R) :-
   numlist_(X,Y,L),
   tfilter(is_even, L, R),
   R = [A,B|_],
   last2_(C,D,R),
   Lb #= A-B,
   Ub #= C+D,
   X in Lb..Ub.
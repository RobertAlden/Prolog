:- use_module(rob_utils, [numlist_/3]).
:- use_module(func_utils, [fold_/4]).

div_(A,B) :- A mod B #= 0.
d(X, S) :-
	Xm1 #= X - 1,
	numlist_(1,Xm1,L1),
	include(div_(X),L1,L2),
	fold_(plus,0,L2,S).


pe21(Lim, Sum) :-
	findall(X, (between(1,Lim,X) ,d(X,Y), d(Y,X)), Res),
	fold_(plus,0,Res, Sum).	

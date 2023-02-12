:- use_module(rob_utils).
:- use_module(func_utils).

div_(A,B) :- A mod B #= 0.
d(X, S) :-
	Xm1 #= X - 1,
	Xm1~>naturals~>
	include(div_(X))~>
	reduce(+)~>S.

pe21(Lim, Sum) :-
	findall(X, (between(1,Lim,X), involution(d,X)), Res),
	reduce(+,Res, Sum).

:- use_module(rob_utils).
:- use_module(func_utils).

pe16(Ex,R) :-
	X #= 2^Ex,
	digits_(X,Dx),
	fold_(plus,0,Dx,R).
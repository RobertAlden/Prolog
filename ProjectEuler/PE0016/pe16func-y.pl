:- use_module(rob_utils).
:- use_module(func_utils).

pe16(Ex,R) :-
	X #= 2^Ex,
	X>-number_digits>-reduce(+)>-R.

iota(N,R):- numlist(1,N,R).

odd(N):- 0 =\= N mod 2.

even(N):- 0 =:= N mod 2.

:- initialization main.
main :-
    iota(10,R),
    include(even, R, Result),
    writeln(Result).

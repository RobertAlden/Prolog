:- initialization main.

fizzbuzz(N, fizzbuzz) :- N mod 3 =:= 0, N mod 5 =:= 0.
fizzbuzz(N, fizz) :- N mod 3 =:= 0, N mod 5 =\= 0.
fizzbuzz(N, buzz) :- N mod 3 =\= 0, N mod 5 =:= 0.
fizzbuzz(N, N) :- N mod 3 =\= 0, N mod 5 =\= 0.

main :-
    numlist(1,100,NumList),
    maplist(fizzbuzz, NumList, FBList),
    maplist(writeln, FBList).


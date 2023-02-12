:- use_module(rob_utils).
:- use_module(func_utils).
:- use_module(prime_utils).

pe_4(S,E,R):-
    numlist_(S,E,Nl),
    findall([A,B], (dif(A,B),member(A,Nl),member(B,Nl)), Cpl),
    Cpl~>maplist(reduce(*))~>
    maplist(number_digits)~>
    include(palindrome)~>
    maplist(commute(number_digits))~>max_list~>R.

answer:-
    pe_4(900,999,R),
    writeln(R).
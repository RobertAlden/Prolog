reverse_([]) --> [].
reverse_([L|Ls]) --> reverse_(Ls), [L].
palindrome(X) :- phrase(reverse_(X),X).
palindrome(X, true) :- palindrome(X).
palindrome(X, false) :- not(palindrome(X)).
mul(A,B,R) :- R #= A*B.
number_digits(Number, 0, [Number]) :- Number in 0..9.
number_digits(Number, N, [Digit|Digits]) :-
        Digit in 0..9,
        N #= N1 + 1,
        Number #= Digit*10^N + Number1,
        Number1 #>= 0,
        N #> 0,
        number_digits(Number1, N1, Digits).

pe_4(S,E,R):-
    numlist(S,E,Nl),
    findall([A,B], (dif(A,B),member(A,Nl),member(B,Nl)), Cpl),
    maplist([X]>>(foldl(mul,X,1)),Cpl,Prl),
    maplist([X]>>(number_digits(X,_)), Prl, Dl),
    tfilter(palindrome,Dl,Pls),
    maplist([X,Y]>>(number_digits(Y,_,X)), Pls, Pal),
    max_list(Pal,R).

answer:-
    pe_4(900,999,R),
    writeln(R).